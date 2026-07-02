#!/usr/bin/env python3
"""Local server for the CorpusReview browser app."""

from __future__ import annotations

import csv
import json
import mimetypes
import os
import re
import ssl
import threading
import urllib.error
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, unquote, urlparse


APP_DIR = Path(__file__).resolve().parent
ROOT = APP_DIR.parent
STATIC_DIR = APP_DIR / "static"
TRANSLATION_DIR = APP_DIR / "translations"
REVIEW_DIR = APP_DIR / "reviews"
# Primary assignment source: the gena handoff-window extractor output.
# Produce it with:  python3 "Python scripts/handoff_window_assignment_gena.py" --all \
#                       --output gena_speech_assignments.csv
# Legacy deterministic CSVs remain as a fallback when the gena CSV is absent.
GENA_CSV = Path(os.environ.get("CORPUSREVIEW_GENA_CSV", str(ROOT / "gena_speech_assignments.csv")))
EVENTS_CSV = ROOT / "deterministic_speech_assignments_2011_2021" / "assignment_events_2011_2021.csv"
SPEECHES_CSV = ROOT / "deterministic_speech_assignments_2011_2021" / "speeches_deterministic_2011_2021.csv"
OPENAI_API_URL = "https://api.openai.com/v1/responses"
DEFAULT_TRANSLATION_MODEL = "gpt-4.1-mini"
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


def corpus_index() -> dict[str, Path]:
    """basename -> path for every corpus file anywhere under ROOT.

    The corpus moved into TN_democratic_cleaned_clean_md/, so a flat
    ROOT-glob no longer finds it; index recursively instead.
    """
    global CORPUS_INDEX
    if CORPUS_INDEX is None:
        CORPUS_INDEX = {
            path.name: path
            for path in ROOT.rglob("*_cleaned_clean.md")
            if APP_DIR not in path.parents
        }
    return CORPUS_INDEX


def safe_root_file(name: str) -> Path | None:
    if "/" in name or "\\" in name:
        return None
    path = corpus_index().get(name)
    if path is not None:
        return path
    path = (ROOT / name).resolve()
    try:
        path.relative_to(ROOT)
    except ValueError:
        return None
    return path


def paired_translation(source_file: str) -> str:
    cached = translation_path(source_file)
    if cached.exists():
        return cached.name
    return ""


def translation_path(source_file: str) -> Path:
    safe_name = source_file.removesuffix(".md") + ".en.md"
    return TRANSLATION_DIR / safe_name


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
    source_path = safe_root_file(source_file)
    if not source_path or not source_path.exists() or not source_file.endswith("_cleaned_clean.md"):
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
    source_path = safe_root_file(source_file)
    if not source_path or not source_path.exists() or not source_file.endswith("_cleaned_clean.md"):
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
    source_path = safe_root_file(source_file)
    if not source_path or not source_path.exists() or not source_file.endswith("_cleaned_clean.md"):
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


def load_manifest() -> list[dict[str, object]]:
    global MANIFEST_CACHE
    if MANIFEST_CACHE is not None:
        return MANIFEST_CACHE

    event_counts: dict[str, int] = {}
    for source_file, events in load_events_by_source().items():
        event_counts[source_file] = len(events)

    docs = []
    for path in sorted(ROOT.glob("*_cleaned_clean.md")):
        source_file = path.name
        translation_file = paired_translation(source_file)
        docs.append(
            {
                "sourceFile": source_file,
                "translationFile": translation_file,
                "documentId": source_file.removesuffix("_cleaned_clean.md"),
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
        if path.startswith("/static/"):
            self.serve_static(unquote(path.removeprefix("/static/")))
            return
        if path == "/api/documents":
            json_response(self, {"documents": load_manifest()})
            return
        if path == "/api/document":
            name = params.get("source", [""])[0]
            source_path = safe_root_file(name)
            if not source_path or not source_path.exists() or not name.endswith("_cleaned_clean.md"):
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
        self.end_headers()
        self.wfile.write(data)


def main() -> None:
    server = ThreadingHTTPServer(("127.0.0.1", 8765), CorpusReviewHandler)
    print("CorpusReview running at http://127.0.0.1:8765")
    server.serve_forever()


if __name__ == "__main__":
    main()
