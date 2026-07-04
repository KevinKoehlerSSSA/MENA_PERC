#!/usr/bin/env python3
"""Local server for the CorpusReview browser app."""

from __future__ import annotations

import csv
import json
import mimetypes
import os
import random
import re
import ssl
import sys
import threading
import urllib.error
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, unquote, urlparse


# speech text fields in the assignments CSV exceed the 128 KiB default
csv.field_size_limit(min(2**31 - 1, sys.maxsize))

APP_DIR = Path(__file__).resolve().parent
ROOT = APP_DIR.parent


def load_dotenv(path: Path) -> None:
    """Minimal .env loader (no dependency): KEY=VALUE lines, # comments.

    Values already present in the environment win, so exported variables
    still override the file.
    """
    if not path.exists():
        return
    for line in path.read_text(encoding="utf-8").splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        key, value = key.strip(), value.strip().strip("'\"")
        if key and value:
            os.environ.setdefault(key, value)


load_dotenv(ROOT / ".env")

STATIC_DIR = APP_DIR / "static"
TRANSLATION_DIR = APP_DIR / "translations"
REVIEW_DIR = APP_DIR / "reviews"
# Primary assignment source: the gena handoff-window extractor output.
# Produce it with:  python3 "Python scripts/handoff_window_assignment.py" --all \
#                       --output gena_speech_assignments.csv
# Legacy deterministic CSVs remain as a fallback when the gena CSV is absent.
GENA_CSV = Path(os.environ.get("CORPUSREVIEW_GENA_CSV", str(ROOT / "gena_speech_assignments.csv")))
EVENTS_CSV = ROOT / "deterministic_speech_assignments_2011_2021" / "assignment_events_2011_2021.csv"
SPEECHES_CSV = ROOT / "deterministic_speech_assignments_2011_2021" / "speeches_deterministic_2011_2021.csv"
OPENAI_API_URL = "https://api.openai.com/v1/responses"
DEFAULT_TRANSLATION_MODEL = "gpt-4.1-mini"
# Seeded 50-speech sample for translation review; persisted (with cached
# translations) so the sample stays stable across server restarts.
SPEECH_SAMPLE_PATH = APP_DIR / "speech_sample_50.json"
SPEECH_SAMPLE_SIZE = int(os.environ.get("CORPUSREVIEW_SAMPLE_SIZE", "50"))
SPEECH_SAMPLE_SEED = int(os.environ.get("CORPUSREVIEW_SAMPLE_SEED", "20260704"))
SPEECH_SAMPLE_JOB_KEY = "__speech_sample__"
MANIFEST_CACHE: list[dict[str, object]] | None = None
EVENTS_CACHE: dict[str, list[dict[str, object]]] | None = None
SPEECHES_CACHE: dict[str, list[dict[str, object]]] | None = None
GENA_CACHE: dict[str, list[dict[str, str]]] | None = None
CORPUS_INDEX: dict[str, Path] | None = None
TRANSLATION_JOBS: dict[str, dict[str, object]] = {}
TRANSLATION_LOCK = threading.Lock()


def make_ssl_context() -> ssl.SSLContext:
    try:
        import certifi

        return ssl.create_default_context(cafile=certifi.where())
    except ImportError:
        return ssl.create_default_context()


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def json_response(handler: BaseHTTPRequestHandler, payload: object, status: int = 200) -> None:
    data = json.dumps(payload, ensure_ascii=False).encode("utf-8")
    handler.send_response(status)
    handler.send_header("Content-Type", "application/json; charset=utf-8")
    handler.send_header("Content-Length", str(len(data)))
    handler.end_headers()
    handler.wfile.write(data)


CORPUS_DIR = Path(os.environ.get("CORPUSREVIEW_CORPUS_DIR", str(ROOT / "TN_democratic_cleaned_clean_md")))


def corpus_index() -> dict[str, Path]:
    """basename -> path for every corpus file.

    The corpus lives in TN_democratic_cleaned_clean_md/ and was renamed:
    files no longer carry the _cleaned_clean suffix, so index every .md
    there.  Legacy checkouts without that directory fall back to the old
    recursive *_cleaned_clean.md scan.
    """
    global CORPUS_INDEX
    if CORPUS_INDEX is None:
        if CORPUS_DIR.is_dir():
            CORPUS_INDEX = {path.name: path for path in CORPUS_DIR.rglob("*.md")}
        else:
            CORPUS_INDEX = {
                path.name: path
                for path in ROOT.rglob("*_cleaned_clean.md")
                if APP_DIR not in path.parents
            }
    return CORPUS_INDEX


def valid_source(name: str) -> Path | None:
    """Resolve a document name to its corpus path; None if not a corpus doc."""
    path = corpus_index().get(name)
    return path if path is not None and path.exists() else None


def paired_translation(source_file: str) -> str:
    cached = translation_path(source_file)
    if cached.exists():
        return cached.name
    return ""


def translation_path(source_file: str) -> Path:
    safe_name = source_file.removesuffix(".md") + ".en.md"
    return TRANSLATION_DIR / safe_name


def line_translation_path(source_file: str) -> Path:
    return TRANSLATION_DIR / (source_file.removesuffix(".md") + ".lines.json")


def load_line_translations(source_file: str) -> dict[str, str]:
    path = line_translation_path(source_file)
    if not path.exists():
        return {}
    try:
        payload = json.loads(read_text(path))
    except json.JSONDecodeError:
        return {}
    return payload if isinstance(payload, dict) else {}


def save_line_translations(source_file: str, translations: dict[str, str]) -> None:
    TRANSLATION_DIR.mkdir(exist_ok=True)
    line_translation_path(source_file).write_text(
        json.dumps(translations, ensure_ascii=False, indent=0), encoding="utf-8"
    )


# Cap for on-demand range translation: keeps a single request cheap and
# bounded, no matter how big the document is.
RANGE_MAX_LINES = int(os.environ.get("CORPUSREVIEW_RANGE_MAX_LINES", "300"))


def translate_lines_json(chunk: list[tuple[int, str]], model: str, api_key: str) -> dict[int, str]:
    """Translate lines keyed by line number, JSON in / JSON out.

    The numbered-plaintext format (translate_chunk) lets the model drift its
    line numbering when OCR breaks sentences across lines — translations end
    up attached to the wrong lines.  A JSON object with line-number keys is
    echoed back far more reliably.
    """
    payload_obj = {str(number): text for number, text in chunk}
    prompt = (
        "Translate this excerpt of Tunisian parliamentary minutes from Arabic into clear English.\n"
        "Input is a JSON object mapping line numbers to consecutive OCR lines; sentences may "
        "continue across lines — translate each line's own content in place, never merge or move "
        "content between keys.\n"
        "Return a JSON object with EXACTLY the same keys and the English translation as each value.\n\n"
        f"{json.dumps(payload_obj, ensure_ascii=False)}"
    )
    request_payload = {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": "You are a careful Arabic-to-English translator for parliamentary transcripts.",
            },
            {"role": "user", "content": prompt},
        ],
        "text": {"format": {"type": "json_object"}},
    }
    request = urllib.request.Request(
        OPENAI_API_URL,
        data=json.dumps(request_payload).encode("utf-8"),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=180, context=make_ssl_context()) as response:
            response_payload = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"OpenAI API error {error.code}: {detail}") from error
    except urllib.error.URLError as error:
        raise RuntimeError(f"Could not reach the OpenAI API: {error}") from error

    text = extract_response_text(response_payload).strip()
    text = re.sub(r"^```(?:json)?\s*|\s*```$", "", text)
    try:
        translated = json.loads(text)
    except json.JSONDecodeError as error:
        raise RuntimeError(f"Model returned unparseable JSON: {error}") from error

    result: dict[int, str] = {}
    if isinstance(translated, dict):
        for key, value in translated.items():
            if str(key) in payload_obj and isinstance(value, str):
                result[int(key)] = value.strip()
    return result


def split_line_paragraphs(lines: list[str]) -> list[tuple[int, int, str]]:
    """(start_line, end_line, joined_text) for each blank-line-separated paragraph."""
    paragraphs: list[tuple[int, int, str]] = []
    block: list[str] = []
    block_start = 0
    for number, line in enumerate(lines, start=1):
        if line.strip():
            if not block:
                block_start = number
            block.append(line.strip())
        elif block:
            paragraphs.append((block_start, number - 1, " ".join(block)))
            block = []
    if block:
        paragraphs.append((block_start, len(lines), " ".join(block)))
    return paragraphs


def translate_line_range(source_file: str, start: int, end: int) -> dict[str, object]:
    """Translate ONLY the paragraphs overlapping lines start..end.

    Translation is per PARAGRAPH (blank-line-separated block), keyed by the
    paragraph's first line: OCR splits sentences mid-line, so line-by-line
    mapping drifts — paragraph units cannot.  Cached paragraphs are never
    re-sent and the range is clamped to RANGE_MAX_LINES, so cost stays
    bounded per click no matter the document size.
    """
    source_path = valid_source(source_file)
    if not source_path:
        raise ValueError("Unknown source document")
    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY is not set")
    model = os.environ.get("CORPUSREVIEW_TRANSLATION_MODEL", DEFAULT_TRANSLATION_MODEL).strip()

    lines = read_text(source_path).splitlines()
    start = max(1, int(start))
    end = min(len(lines), int(end))
    if end < start:
        raise ValueError("Empty line range")
    end = min(end, start + RANGE_MAX_LINES - 1)

    cache = load_line_translations(source_file)
    selected = [
        paragraph for paragraph in split_line_paragraphs(lines)
        if paragraph[1] >= start and paragraph[0] <= end
    ]
    pending = [
        (par_start, par_end, text)
        for par_start, par_end, text in selected
        if str(par_start) not in cache
    ]

    # Split pending paragraphs into <=9000-char API chunks.
    chunks: list[list[tuple[int, int, str]]] = []
    chunk: list[tuple[int, int, str]] = []
    chunk_chars = 0
    for item in pending:
        item_chars = len(item[2]) + 12
        if chunk and chunk_chars + item_chars > 9000:
            chunks.append(chunk)
            chunk = []
            chunk_chars = 0
        chunk.append(item)
        chunk_chars += item_chars
    if chunk:
        chunks.append(chunk)

    for chunk in chunks:
        translated = translate_lines_json(
            [(par_start, text) for par_start, _, text in chunk], model, api_key
        )
        for par_start, par_end, _ in chunk:
            text = translated.get(par_start, "")
            if not text:
                continue
            cache[str(par_start)] = text
            # mark continuation lines so they are never re-requested
            for number in range(par_start + 1, par_end + 1):
                cache.setdefault(str(number), "")
        save_line_translations(source_file, cache)

    if selected:
        start = min(start, selected[0][0])
        end = max(end, selected[-1][1])
    return {
        "start": start,
        "end": end,
        "requestedLines": len(pending),
        "translatedTotal": sum(1 for value in cache.values() if value),
        "lines": {number: cache.get(str(number), "") for number in range(start, end + 1)},
        "model": model,
    }


def review_path(source_file: str) -> Path:
    safe_name = source_file.removesuffix(".md") + ".review.md"
    return REVIEW_DIR / safe_name


def empty_review(source_file: str) -> dict[str, object]:
    return {"sourceFile": source_file, "decisions": {}, "additionalHandoffs": []}


def load_review(source_file: str) -> dict[str, object]:
    path = review_path(source_file)
    if not path.exists():
        return empty_review(source_file)
    text = read_text(path)
    match = re.search(r"```json\s*(\{.*?\})\s*```", text, re.S)
    if not match:
        return empty_review(source_file)
    try:
        payload = json.loads(match.group(1))
    except json.JSONDecodeError:
        return empty_review(source_file)
    if not isinstance(payload, dict):
        return empty_review(source_file)
    payload.setdefault("sourceFile", source_file)
    payload.setdefault("decisions", {})
    payload.setdefault("additionalHandoffs", [])
    payload["reviewFile"] = path.name
    return payload


def write_review(source_file: str, review: dict[str, object]) -> dict[str, object]:
    if not valid_source(source_file):
        raise ValueError("Unknown source document")

    decisions = review.get("decisions", {})
    additions = review.get("additionalHandoffs", [])
    if not isinstance(decisions, dict):
        decisions = {}
    if not isinstance(additions, list):
        additions = []

    normalized = {
        "sourceFile": source_file,
        "decisions": {str(key): value for key, value in decisions.items() if value in ("accepted", "rejected")},
        "additionalHandoffs": additions,
    }
    accepted = sum(1 for value in normalized["decisions"].values() if value == "accepted")
    rejected = sum(1 for value in normalized["decisions"].values() if value == "rejected")
    REVIEW_DIR.mkdir(exist_ok=True)
    path = review_path(source_file)
    body = [
        "# CorpusReview Handoff Review",
        "",
        f"- Source file: `{source_file}`",
        f"- Accepted detected handoffs: {accepted}",
        f"- Rejected detected handoffs: {rejected}",
        f"- Additional handoffs: {len(additions)}",
        "",
        "## Review Data",
        "",
        "```json",
        json.dumps(normalized, ensure_ascii=False, indent=2),
        "```",
        "",
    ]
    path.write_text("\n".join(body), encoding="utf-8")
    normalized["reviewFile"] = path.name
    return normalized


def chunk_numbered_lines(lines: list[str], max_chars: int = 9000) -> list[list[tuple[int, str]]]:
    chunks: list[list[tuple[int, str]]] = []
    current: list[tuple[int, str]] = []
    current_chars = 0
    for index, line in enumerate(lines, start=1):
        item_chars = len(line) + 12
        if current and current_chars + item_chars > max_chars:
            chunks.append(current)
            current = []
            current_chars = 0
        current.append((index, line))
        current_chars += item_chars
    if current:
        chunks.append(current)
    return chunks


def extract_response_text(payload: dict[str, object]) -> str:
    text = payload.get("output_text")
    if isinstance(text, str):
        return text
    pieces: list[str] = []
    for item in payload.get("output", []):
        if not isinstance(item, dict):
            continue
        for content in item.get("content", []):
            if isinstance(content, dict) and content.get("type") == "output_text":
                value = content.get("text")
                if isinstance(value, str):
                    pieces.append(value)
    return "\n".join(pieces)


def parse_numbered_translation(text: str, expected: list[tuple[int, str]]) -> list[str]:
    by_line: dict[int, str] = {}
    for raw in text.splitlines():
        match = re.match(r"^\s*(\d+)\s*[\t|:.-]\s*(.*)$", raw)
        if match:
            by_line[int(match.group(1))] = match.group(2).strip()
    return [by_line.get(line_no, "") for line_no, _ in expected]


def translate_chunk(chunk: list[tuple[int, str]], model: str, api_key: str) -> list[str]:
    numbered = "\n".join(f"{line_no}\t{text}" for line_no, text in chunk)
    prompt = (
        "Translate these Tunisian parliamentary minutes from Arabic into clear English.\n"
        "Preserve Markdown structure where possible. Return exactly one line per input line.\n"
        "Each output line must start with the original line number, a tab, then the English translation.\n"
        "For blank input lines, return the line number and then nothing after the tab.\n\n"
        f"{numbered}"
    )
    request_payload = {
        "model": model,
        "input": [
            {
                "role": "system",
                "content": "You are a careful Arabic-to-English translator for parliamentary transcripts.",
            },
            {"role": "user", "content": prompt},
        ],
    }
    request = urllib.request.Request(
        OPENAI_API_URL,
        data=json.dumps(request_payload).encode("utf-8"),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=120, context=make_ssl_context()) as response:
            response_payload = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"OpenAI API error {error.code}: {detail}") from error
    except urllib.error.URLError as error:
        raise RuntimeError(
            "Could not reach the OpenAI API. If the message mentions CERTIFICATE_VERIFY_FAILED, "
            "run Python's Install Certificates.command or install certifi for this Python."
            f" Details: {error}"
        ) from error
    return parse_numbered_translation(extract_response_text(response_payload), chunk)


def update_translation_job(source_file: str, **updates: object) -> None:
    with TRANSLATION_LOCK:
        job = TRANSLATION_JOBS.setdefault(source_file, {})
        job.update(updates)


def get_translation_job(source_file: str) -> dict[str, object] | None:
    with TRANSLATION_LOCK:
        job = TRANSLATION_JOBS.get(source_file)
        return dict(job) if job else None


def generate_translation(source_file: str) -> dict[str, object]:
    global MANIFEST_CACHE
    source_path = valid_source(source_file)
    if not source_path:
        raise ValueError("Unknown source document")
    api_key = os.environ.get("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY is not set")
    model = os.environ.get("CORPUSREVIEW_TRANSLATION_MODEL", DEFAULT_TRANSLATION_MODEL).strip()
    lines = read_text(source_path).splitlines()
    chunks = chunk_numbered_lines(lines)
    translated: list[str] = []
    TRANSLATION_DIR.mkdir(exist_ok=True)
    target = translation_path(source_file)
    partial = target.with_suffix(target.suffix + ".partial")
    update_translation_job(source_file, totalChunks=len(chunks), doneChunks=0, model=model)
    for index, chunk in enumerate(chunks, start=1):
        translated.extend(translate_chunk(chunk, model, api_key))
        partial.write_text("\n".join(translated) + "\n", encoding="utf-8")
        update_translation_job(source_file, doneChunks=index)
    target.write_text("\n".join(translated) + "\n", encoding="utf-8")
    MANIFEST_CACHE = None
    if partial.exists():
        partial.unlink()
    return {"translationFile": target.name, "translationText": read_text(target), "model": model}


def run_translation_job(source_file: str) -> None:
    try:
        result = generate_translation(source_file)
        update_translation_job(
            source_file,
            status="done",
            error="",
            translationFile=result["translationFile"],
            model=result["model"],
        )
    except Exception as error:
        update_translation_job(source_file, status="error", error=str(error))


def start_translation_job(source_file: str) -> dict[str, object]:
    if not valid_source(source_file):
        raise ValueError("Unknown source document")
    cached = translation_path(source_file)
    if cached.exists():
        return {
            "status": "done",
            "translationFile": cached.name,
            "translationText": read_text(cached),
            "cached": True,
        }
    existing = get_translation_job(source_file)
    if existing and existing.get("status") == "running":
        return existing
    update_translation_job(source_file, status="running", error="", doneChunks=0, totalChunks=0)
    thread = threading.Thread(target=run_translation_job, args=(source_file,), daemon=True)
    thread.start()
    return get_translation_job(source_file) or {"status": "running"}


def translation_status(source_file: str) -> dict[str, object]:
    cached = translation_path(source_file)
    job = get_translation_job(source_file) or {}
    if cached.exists():
        return {
            **job,
            "status": "done",
            "translationFile": cached.name,
            "translationText": read_text(cached),
        }
    partial = cached.with_suffix(cached.suffix + ".partial")
    if partial.exists():
        job.setdefault("status", "partial")
        job["partialText"] = read_text(partial)
    return job or {"status": "idle"}


def translate_plain_text(text: str, model: str, api_key: str) -> str:
    """Translate a single speech (plain text, no line numbering).

    Long speeches are split on paragraph boundaries into <=8000-char parts
    so each request stays well inside model limits.
    """
    parts: list[str] = []
    current = ""
    for para in text.split("\n\n"):
        if current and len(current) + len(para) + 2 > 8000:
            parts.append(current)
            current = para
        else:
            current = f"{current}\n\n{para}" if current else para
    if current:
        parts.append(current)

    translated: list[str] = []
    for part in parts:
        request_payload = {
            "model": model,
            "input": [
                {
                    "role": "system",
                    "content": "You are a careful Arabic-to-English translator for parliamentary transcripts.",
                },
                {
                    "role": "user",
                    "content": (
                        "Translate this excerpt of Tunisian parliamentary minutes from Arabic "
                        "into clear English. Preserve paragraph breaks. Return only the translation.\n\n"
                        f"{part}"
                    ),
                },
            ],
        }
        request = urllib.request.Request(
            OPENAI_API_URL,
            data=json.dumps(request_payload).encode("utf-8"),
            headers={
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json",
            },
            method="POST",
        )
        try:
            with urllib.request.urlopen(request, timeout=120, context=make_ssl_context()) as response:
                response_payload = json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            detail = error.read().decode("utf-8", errors="replace")
            raise RuntimeError(f"OpenAI API error {error.code}: {detail}") from error
        except urllib.error.URLError as error:
            raise RuntimeError(f"Could not reach the OpenAI API: {error}") from error
        translated.append(extract_response_text(response_payload).strip())
    return "\n\n".join(translated)


SPEECH_SAMPLE_FIELDS = [
    "speech_id",
    "source_file",
    "inferred_date",
    "legislature_period",
    "speaker_target_clean",
    "speaker_type",
    "speaker_role",
    "mp_name_ar",
    "mp_name_transliterated",
    "mp_party_ar",
    "assignment_method",
    "assignment_confidence",
    "word_count",
    "text",
]


def build_speech_sample() -> list[dict[str, object]]:
    """Seeded random sample of speeches from the assignments CSV.

    Only substantive rows qualify: real speech text of at least 20 words
    (drops procedural one-liners the reviewers don't need to read).
    """
    pool: list[dict[str, str]] = []
    for rows in load_gena_by_source().values():
        for row in rows:
            try:
                words = int(row.get("word_count") or 0)
            except ValueError:
                words = 0
            if words >= 20 and (row.get("text") or "").strip():
                pool.append(row)
    rng = random.Random(SPEECH_SAMPLE_SEED)
    picked = rng.sample(pool, min(SPEECH_SAMPLE_SIZE, len(pool)))
    picked.sort(key=lambda row: (row.get("inferred_date") or "", row.get("speech_id") or ""))
    return [
        {**{field: row.get(field, "") for field in SPEECH_SAMPLE_FIELDS}, "translation": ""}
        for row in picked
    ]


def load_speech_sample() -> list[dict[str, object]]:
    if SPEECH_SAMPLE_PATH.exists():
        try:
            payload = json.loads(read_text(SPEECH_SAMPLE_PATH))
            if isinstance(payload, list) and payload:
                return payload
        except json.JSONDecodeError:
            pass
    sample = build_speech_sample()
    save_speech_sample(sample)
    return sample


def save_speech_sample(sample: list[dict[str, object]]) -> None:
    SPEECH_SAMPLE_PATH.write_text(
        json.dumps(sample, ensure_ascii=False, indent=1), encoding="utf-8"
    )


def run_speech_sample_translation() -> None:
    try:
        api_key = os.environ.get("OPENAI_API_KEY", "").strip()
        if not api_key:
            raise RuntimeError("OPENAI_API_KEY is not set")
        model = os.environ.get("CORPUSREVIEW_TRANSLATION_MODEL", DEFAULT_TRANSLATION_MODEL).strip()
        sample = load_speech_sample()
        pending = [item for item in sample if not item.get("translation")]
        update_translation_job(
            SPEECH_SAMPLE_JOB_KEY,
            totalChunks=len(pending), doneChunks=0, model=model,
        )
        for index, item in enumerate(pending, start=1):
            item["translation"] = translate_plain_text(str(item.get("text", "")), model, api_key)
            save_speech_sample(sample)
            update_translation_job(SPEECH_SAMPLE_JOB_KEY, doneChunks=index)
        update_translation_job(SPEECH_SAMPLE_JOB_KEY, status="done", error="")
    except Exception as error:
        update_translation_job(SPEECH_SAMPLE_JOB_KEY, status="error", error=str(error))


def start_speech_sample_translation() -> dict[str, object]:
    existing = get_translation_job(SPEECH_SAMPLE_JOB_KEY)
    if existing and existing.get("status") == "running":
        return existing
    update_translation_job(
        SPEECH_SAMPLE_JOB_KEY, status="running", error="", doneChunks=0, totalChunks=0
    )
    thread = threading.Thread(target=run_speech_sample_translation, daemon=True)
    thread.start()
    return get_translation_job(SPEECH_SAMPLE_JOB_KEY) or {"status": "running"}


def load_manifest() -> list[dict[str, object]]:
    global MANIFEST_CACHE
    if MANIFEST_CACHE is not None:
        return MANIFEST_CACHE

    event_counts: dict[str, int] = {}
    for source_file, events in load_events_by_source().items():
        event_counts[source_file] = len(events)

    docs = []
    for source_file in sorted(corpus_index()):
        translation_file = paired_translation(source_file)
        docs.append(
            {
                "sourceFile": source_file,
                "translationFile": translation_file,
                "documentId": source_file.removesuffix(".md").removesuffix("_cleaned_clean"),
                "handoffCount": event_counts.get(source_file, 0),
                "hasTranslation": bool(translation_file),
            }
        )
    MANIFEST_CACHE = docs
    return docs


def load_gena_by_source() -> dict[str, list[dict[str, str]]]:
    """Rows from the gena handoff-window extractor, grouped by source file."""
    global GENA_CACHE
    if GENA_CACHE is not None:
        return GENA_CACHE

    by_source: dict[str, list[dict[str, str]]] = {}
    if GENA_CSV.exists():
        with GENA_CSV.open(encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                by_source.setdefault(row.get("source_file", ""), []).append(row)
    GENA_CACHE = by_source
    return by_source


def load_events_by_source() -> dict[str, list[dict[str, object]]]:
    global EVENTS_CACHE
    if EVENTS_CACHE is not None:
        return EVENTS_CACHE

    by_source: dict[str, list[dict[str, object]]] = {}

    gena = load_gena_by_source()
    if gena:
        # One event per detected handoff cue (gena's determination).
        for source_file, rows in gena.items():
            for row in rows:
                by_source.setdefault(source_file, []).append(
                    {
                        "line": int(row.get("evidence_line") or 0),
                        "candidateRaw": row.get("speaker_target_raw", ""),
                        "candidateClean": row.get("speaker_target_clean", ""),
                        "speakerType": row.get("speaker_type", ""),
                        "speakerRole": row.get("speaker_role", ""),
                        "mpId": row.get("mp_id", ""),
                        "mpNameAr": row.get("mp_name_ar", ""),
                        "assignmentMethod": row.get("assignment_method", ""),
                        "assignmentConfidence": row.get("assignment_confidence", ""),
                        "evidenceText": row.get("evidence_text", ""),
                    }
                )
        EVENTS_CACHE = by_source
        return by_source

    if not EVENTS_CSV.exists():
        EVENTS_CACHE = by_source
        return by_source
    with EVENTS_CSV.open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            source_file = row.get("source_file", "")
            by_source.setdefault(source_file, []).append(
                {
                    "line": int(row["line"]),
                    "candidateRaw": row.get("candidate_raw", ""),
                    "candidateClean": row.get("candidate_clean", ""),
                    "speakerType": row.get("speaker_type", ""),
                    "mpId": row.get("mp_id", ""),
                    "mpNameAr": row.get("mp_name_ar", ""),
                    "assignmentMethod": row.get("assignment_method", ""),
                    "assignmentConfidence": row.get("assignment_confidence", ""),
                    "evidenceText": row.get("evidence_text", ""),
                }
            )
    EVENTS_CACHE = by_source
    return by_source


def load_events(source_file: str) -> list[dict[str, object]]:
    return load_events_by_source().get(source_file, [])


def load_speeches_by_source() -> dict[str, list[dict[str, object]]]:
    global SPEECHES_CACHE
    if SPEECHES_CACHE is not None:
        return SPEECHES_CACHE

    by_source: dict[str, list[dict[str, object]]] = {}

    gena = load_gena_by_source()
    if gena:
        for source_file, rows in gena.items():
            for row in rows:
                by_source.setdefault(source_file, []).append(
                    {
                        "startLine": int(row.get("start_line") or 0),
                        "endLine": int(row.get("end_line") or 0),
                        "evidenceLine": int(row.get("evidence_line") or 0),
                        "speakerTargetClean": row.get("speaker_target_clean", ""),
                        "speakerType": row.get("speaker_type", ""),
                        "speakerRole": row.get("speaker_role", ""),
                        "needsReview": row.get("needs_review", ""),
                        "text": row.get("text", ""),
                    }
                )
        SPEECHES_CACHE = by_source
        return by_source

    if not SPEECHES_CSV.exists():
        SPEECHES_CACHE = by_source
        return by_source
    with SPEECHES_CSV.open(encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            source_file = row.get("source_file", "")
            by_source.setdefault(source_file, []).append(
                {
                    "startLine": int(row["start_line"]),
                    "endLine": int(row["end_line"]),
                    "evidenceLine": int(row["evidence_line"]),
                    "speakerTargetClean": row.get("speaker_target_clean", ""),
                    "speakerType": row.get("speaker_type", ""),
                    "needsReview": row.get("needs_review", ""),
                    "text": row.get("text", ""),
                }
            )
    SPEECHES_CACHE = by_source
    return by_source


def load_speeches(source_file: str) -> list[dict[str, object]]:
    return load_speeches_by_source().get(source_file, [])


class CorpusReviewHandler(BaseHTTPRequestHandler):
    def log_message(self, format: str, *args: object) -> None:
        return

    def do_GET(self) -> None:
        parsed = urlparse(self.path)
        path = parsed.path
        params = parse_qs(parsed.query)

        if path == "/":
            self.serve_static("index.html")
            return
        if path == "/speeches":
            self.serve_static("speeches.html")
            return
        if path.startswith("/static/"):
            self.serve_static(unquote(path.removeprefix("/static/")))
            return
        if path == "/api/speech-sample":
            job = get_translation_job(SPEECH_SAMPLE_JOB_KEY) or {}
            json_response(self, {"speeches": load_speech_sample(), "job": job})
            return
        if path == "/api/documents":
            json_response(self, {"documents": load_manifest()})
            return
        if path == "/api/document":
            name = params.get("source", [""])[0]
            source_path = valid_source(name)
            if not source_path:
                json_response(self, {"error": "Unknown source document"}, 404)
                return
            translation_name = paired_translation(name)
            translation_text = ""
            if translation_name:
                translation_text = read_text(TRANSLATION_DIR / translation_name)
            payload = {
                "sourceFile": name,
                "translationFile": translation_name,
                "rawText": read_text(source_path),
                "translationText": translation_text,
                "lineTranslations": load_line_translations(name),
                "events": load_events(name),
                "speeches": load_speeches(name),
                "review": load_review(name),
            }
            json_response(self, payload)
            return
        if path == "/api/translation-status":
            name = params.get("source", [""])[0]
            json_response(self, translation_status(name))
            return
        json_response(self, {"error": "Not found"}, 404)

    def do_POST(self) -> None:
        parsed = urlparse(self.path)
        if parsed.path == "/api/review":
            self.handle_review_post()
            return
        if parsed.path == "/api/speech-sample/translate":
            try:
                json_response(self, start_speech_sample_translation())
            except Exception as error:
                json_response(self, {"error": str(error)}, 500)
            return
        if parsed.path == "/api/translate-range":
            length = int(self.headers.get("Content-Length", "0"))
            payload = json.loads(self.rfile.read(length).decode("utf-8") or "{}")
            try:
                json_response(
                    self,
                    translate_line_range(
                        str(payload.get("source", "")),
                        int(payload.get("start", 0)),
                        int(payload.get("end", 0)),
                    ),
                )
            except ValueError as error:
                json_response(self, {"error": str(error)}, 400)
            except Exception as error:
                json_response(self, {"error": str(error)}, 500)
            return
        if parsed.path != "/api/translate":
            json_response(self, {"error": "Not found"}, 404)
            return
        length = int(self.headers.get("Content-Length", "0"))
        payload = json.loads(self.rfile.read(length).decode("utf-8") or "{}")
        source_file = payload.get("source", "")
        if not isinstance(source_file, str):
            json_response(self, {"error": "Invalid source document"}, 400)
            return
        try:
            json_response(self, start_translation_job(source_file))
        except ValueError as error:
            json_response(self, {"error": str(error)}, 404)
        except RuntimeError as error:
            json_response(self, {"error": str(error)}, 500)
        except Exception as error:
            json_response(self, {"error": str(error)}, 500)

    def handle_review_post(self) -> None:
        length = int(self.headers.get("Content-Length", "0"))
        payload = json.loads(self.rfile.read(length).decode("utf-8") or "{}")
        source_file = payload.get("sourceFile", "")
        if not isinstance(source_file, str):
            json_response(self, {"error": "Invalid source document"}, 400)
            return
        try:
            json_response(self, write_review(source_file, payload))
        except ValueError as error:
            json_response(self, {"error": str(error)}, 404)
        except Exception as error:
            json_response(self, {"error": str(error)}, 500)

    def serve_static(self, name: str) -> None:
        path = (STATIC_DIR / name).resolve()
        try:
            path.relative_to(STATIC_DIR)
        except ValueError:
            json_response(self, {"error": "Not found"}, 404)
            return
        if not path.exists() or not path.is_file():
            json_response(self, {"error": "Not found"}, 404)
            return
        content_type = mimetypes.guess_type(path.name)[0] or "application/octet-stream"
        data = path.read_bytes()
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)


def main() -> None:
    server = ThreadingHTTPServer(("127.0.0.1", 8765), CorpusReviewHandler)
    print("CorpusReview running at http://127.0.0.1:8765")
    server.serve_forever()


if __name__ == "__main__":
    main()
