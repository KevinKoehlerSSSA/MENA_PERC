#!/usr/bin/env python3
"""Cue-window speaker assignment for Tunisian parliamentary minutes.

This is a test implementation of a handoff-window strategy. It reads
pre-speaker-assignment `*_cleaned_clean.md` files, detects explicit chair
handoff cues, extracts the named candidate, and captures the speech window
following that cue.

Default run mode samples 10 in-scope files and writes `test_corpus.csv`.
Use `--all` to process all in-scope files, or `--sample-size N`.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import random
import re
import sys
from dataclasses import dataclass
from difflib import SequenceMatcher
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT = ROOT / "test_corpus.csv"
ROSTER_PATH = Path(
    os.environ.get(
        "TUNISIA_MP_ROSTER",
        "/Users/kevin/SSSUP Dropbox/Kevin Koehler/MENA-PERC/Publications/"
        "Controversies/Tunisia MPs/RollCalls/Tunisian Parliamentary Dataset.csv",
    )
)

YEAR_MIN = 2011
YEAR_MAX = 2021

LEADING_DATE = re.compile(r"^(?P<year>20\d{2}|19\d{2})[-_](?P<month>\d{1,2})[-_](?P<day>\d{1,2})")
LEADING_COMPACT_DATE = re.compile(r"^(?P<year>20\d{2}|19\d{2})(?P<month>\d{2})(?P<day>\d{2})")
LEADING_SESSION_RANGE = re.compile(r"^(?P<start>20\d{2})-(?P<end>20\d{2})_")
MUD_AWALAT_2011 = re.compile(r"^2011_Mudawalat_2011_")
BULLETIN_DATE = re.compile(
    r"bulletin_officiel_.*?[-_](?P<day>\d{1,2})[-_](?P<month>\d{1,2})[-_](?P<year>20\d{2})",
    re.I,
)

AR_DIACRITICS = re.compile(r"[\u064b-\u065f\u0670\u0640]")
NON_AR_NUM_SPACE = re.compile(r"[^\u0600-\u06FF0-9 ]+")
SPACES = re.compile(r"\s+")

HONORIFICS = (
    "السيد",
    "السيده",
    "سيد",
    "سيده",
    "النائب",
    "النائبه",
    "نائب",
    "نائبه",
    "الزميل",
    "الزميله",
    "زميل",
    "زميله",
    "الاستاذ",
    "الاستاذه",
    "استاذ",
    "استاذه",
    "الدكتور",
    "الدكتوره",
    "دكتور",
    "دكتوره",
)

TITLE_WORDS = {
    "رئيس",
    "رئيسه",
    "مجلس",
    "نواب",
    "الشعب",
    "الوطني",
    "التاسيسي",
    "لجنه",
    "اللجنه",
    "وزير",
    "وزيره",
    "كاتب",
    "كاتبه",
    "الدوله",
    "مقرر",
    "مقرره",
    "عضو",
    "عضوه",
    "الحكومه",
    "الوزاره",
    "الفصل",
    "الجلسه",
    "الحضور",
}

OFFICE_MARKERS = re.compile(
    r"(?:رئيس|رئيسة|مجلس|نواب|الشعب|الوطني|التأسيسي|لجنة|اللجنة|وزير|وزيرة|"
    r"كاتب الدولة|كاتبة الدولة|مقرر|مقررة|عضو|عضوة|الحكومة|الوزارة|الفصل|الجلسة)"
)

NO_AR_LETTER_AFTER = r"(?![\u0600-\u06FF])"
TFADEL = r"تفض[\u064b-\u065f\u0670\u0640]*ل" + NO_AR_LETTER_AFTER
TFADELI = r"تفض[\u064b-\u065f\u0670\u0640]*لي" + NO_AR_LETTER_AFTER
FALYATAFADEL = r"فليتفض[\u064b-\u065f\u0670\u0640]*ل" + NO_AR_LETTER_AFTER
FALTATAFADEL = r"فلتتفض[\u064b-\u065f\u0670\u0640]*ل" + NO_AR_LETTER_AFTER
TFADEL_WORDS = rf"(?:{TFADEL}|{TFADELI}|{FALYATAFADEL}|{FALTATAFADEL})"

HANDOFF_PATTERNS = [
    re.compile(
        r"(?:الكلمة|أحيل الكلمة|أُحيل الكلمة|احيل الكلمة|نمرر الكلمة|نمر الكلمة|"
        r"نمرّر الكلمة|نعطي الكلمة|أعطي الكلمة|اعطي الكلمة)\s+"
        r"(?:الآن\s+)?(?:إلى\s+|الى\s+)?"
        r"(?:للسيدة|للسيد|للنائبة|للنائب|للزميلة|للزميل|للأستاذة|للأستاذ|"
        r"للاستاذة|للاستاذ|ل)?\s*(?P<name>[^،.؛:\n]+)"
    ),
    re.compile(
        r"(?:نقطة نظام)\s+(?:إلى\s+|الى\s+)?"
        r"(?:للسيدة|للسيد|للنائبة|للنائب|للزميلة|للزميل|ل)?\s*"
        r"(?P<name>[^،.؛:\n]+)"
    ),
    re.compile(
        TFADEL_WORDS + r"\s+(?:يا\s+)?"
        r"(?:السيدة|السيد|النائبة|النائب|الزميلة|الزميل|الأستاذة|الأستاذ)?\s*"
        r"(?P<name>[^،.؛:\n]+)"
    ),
    re.compile(
        r"(?:السيدة|السيد|النائبة|النائب|الزميلة|الزميل|الأستاذة|الأستاذ)\s+"
        r"(?P<name>[^،.؛:\n]+?)\s+" + TFADEL_WORDS
    ),
]

STOP_AFTER_CANDIDATE = re.compile(
    rf"\s+(?:{TFADEL}|{TFADELI}|تفضيلي|{FALYATAFADEL}|{FALTATAFADEL}|لديه|لديها|له|لها|خمس|ثلاث|"
    r"ست|سبع|ثماني|تسع|دقيقتان|دقيقة|دقائق|غير موجود|في نقطة|نقطة نظام|"
    r"للدفاع|للدفاع عن|ثم|لكن|قبل|بعد|نمر|نمرر|نواصل|$)"
)

SPEECH_OPENERS = re.compile(
    r"^(?:بسم الله|شكراً|شكرا|سيدي الرئيس|سيد الرئيس|السيدة الرئيسة|سيده الرئيسه|"
    r"حضرات|زملائي|زميلاتي|مرحبا|صباح الخير|يومكم سعيد)"
)

NOISE_LINE = re.compile(
    r"^(?:صفحة\s+\d+|\d{3,}|صد\s+\d+|مداولات مجلس|الدورة العادية|المدة النيابية|"
    r"##\s+|افتتاح الجلسة|استئناف الجلسة|رفع الجلسة)\s*$"
)

SECTION_HEADER = re.compile(r"^(?:#{1,6}\s+|(?:الباب|القسم|الفرع|الفصل)\s+\S+)")
VOTE_OR_ARTICLE = re.compile(
    r"(?:نمر إلى التصويت|نمر للتصويت|التصويت|تمت الموافقة|يعرض مباشرة على التصويت|"
    r"الفصل\s+\S+|مشروع قانون|مشروع الميزانية)"
)
REPORT_HEADER = re.compile(r"(?:تقرير\s+لجنة|حول\s+مشروع\s+(?:قانون|ميزانية)|أعمال\s*اللجنة)")


@dataclass
class FileMeta:
    source_file: str
    document_id: str
    year_start: int | None
    year_end: int | None
    inferred_date: str | None
    include: bool
    reason: str


@dataclass
class Paragraph:
    start_line: int
    end_line: int
    text: str


@dataclass
class MPRecord:
    mp_id: str
    name_ar: str
    name_norm: str
    name_transliterated: str
    gender: str
    period: str
    party_ar: str
    district_ar: str


@dataclass
class MatchResult:
    speaker_type: str
    method: str
    confidence: float
    mp: MPRecord | None = None


@dataclass
class Cue:
    raw_name: str
    clean_name: str
    start: int
    end: int
    method: str


def valid_date(year: int, month: int, day: int) -> str | None:
    if not 1 <= month <= 12 or not 1 <= day <= 31:
        return None
    return f"{year:04d}-{month:02d}-{day:02d}"


def infer_file_meta(path: Path) -> FileMeta:
    name = path.name
    document_id = name.removesuffix("_cleaned_clean.md")

    if MUD_AWALAT_2011.match(name):
        return FileMeta(name, document_id, 2011, 2014, None, True, "2011_mudawalat")

    year_start = None
    year_end = None
    inferred_date = None

    m = LEADING_DATE.match(name)
    if m:
        year_start = year_end = int(m.group("year"))
        inferred_date = valid_date(year_start, int(m.group("month")), int(m.group("day")))
    else:
        m = LEADING_COMPACT_DATE.match(name)
        if m:
            year_start = year_end = int(m.group("year"))
            inferred_date = valid_date(year_start, int(m.group("month")), int(m.group("day")))

    if year_start is None:
        m = LEADING_SESSION_RANGE.match(name)
        if m:
            year_start = int(m.group("start"))
            year_end = int(m.group("end"))

    if year_start is None:
        m = BULLETIN_DATE.search(name)
        if m:
            year_start = year_end = int(m.group("year"))
            inferred_date = valid_date(year_start, int(m.group("month")), int(m.group("day")))

    include = bool(year_start and year_end and year_start <= YEAR_MAX and year_end >= YEAR_MIN)
    reason = "in_scope_year" if include else "outside_2011_2021_or_unknown_date"
    return FileMeta(name, document_id, year_start, year_end, inferred_date, include, reason)


def legislature_period_for_file(meta: FileMeta) -> str:
    name = meta.source_file
    if name.startswith("2011_Mudawalat"):
        return "2011-2014"
    if name.startswith("2020-2021") or name.startswith("2019-10"):
        return "2019-2024"
    if meta.year_start and 2014 <= meta.year_start <= 2019:
        return "2014-2019"
    if meta.year_start == 2011:
        return "2011-2014"
    return ""


def normalize_arabic(text: str) -> str:
    text = AR_DIACRITICS.sub("", text)
    replacements = {
        "أ": "ا",
        "إ": "ا",
        "آ": "ا",
        "ٱ": "ا",
        "ى": "ي",
        "ة": "ه",
        "ؤ": "و",
        "ئ": "ي",
    }
    for src, dst in replacements.items():
        text = text.replace(src, dst)
    text = NON_AR_NUM_SPACE.sub(" ", text)
    return SPACES.sub(" ", text).strip()


def strip_honorifics(text: str) -> str:
    norm = normalize_arabic(text)
    changed = True
    while changed:
        changed = False
        for honorific in HONORIFICS:
            h = normalize_arabic(honorific)
            if norm.startswith(h + " "):
                norm = norm[len(h) + 1 :].strip()
                changed = True
    return norm


def clean_candidate(candidate: str) -> str:
    candidate = SPACES.sub(" ", candidate.replace("#", " ")).strip(" ،.؛:-")
    candidate = STOP_AFTER_CANDIDATE.split(candidate, 1)[0]
    candidate = re.sub(r"\b(?:وذلك|والآن|الآن)\b", " ", candidate).strip()
    return SPACES.sub(" ", candidate).strip(" ،.؛:-")


def read_roster(path: Path) -> dict[str, list[MPRecord]]:
    by_period: dict[str, list[MPRecord]] = {}
    if not path.exists():
        print(f"warning: roster not found: {path}", file=sys.stderr)
        return by_period

    with path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            name_ar = (row.get("Name (Arabic)") or "").strip()
            period = (row.get("Parliamentary Period") or "").strip()
            if not name_ar or not period:
                continue
            rec = MPRecord(
                mp_id=(row.get("M.P. ID Number") or "").strip(),
                name_ar=name_ar,
                name_norm=strip_honorifics(name_ar),
                name_transliterated=(row.get("Name (transliterated)") or "").strip(),
                gender=(row.get("Gender") or "").strip(),
                period=period,
                party_ar=(row.get("Politcal Party (Arabic)") or "").strip(),
                district_ar=(row.get("Electoral District (Arabic)") or "").strip(),
            )
            by_period.setdefault(period, []).append(rec)
    return by_period


def candidate_is_office(candidate: str) -> bool:
    norm = strip_honorifics(candidate)
    words = set(norm.split())
    return bool(OFFICE_MARKERS.search(candidate)) or bool(words & TITLE_WORDS)


def link_candidate(candidate: str, period: str, roster: dict[str, list[MPRecord]]) -> MatchResult:
    candidate_norm = strip_honorifics(candidate)
    if not candidate_norm:
        return MatchResult("unassigned", "empty_candidate", 0.0)

    candidates = roster.get(period, [])
    exact = [mp for mp in candidates if mp.name_norm == candidate_norm]
    if len(exact) == 1:
        conf = 1.0 if candidate_norm == normalize_arabic(candidate) else 0.95
        return MatchResult("mp", "exact_period_roster", conf, exact[0])

    contained = [mp for mp in candidates if mp.name_norm and mp.name_norm in candidate_norm]
    if len(contained) == 1:
        return MatchResult("mp", "roster_name_contained_in_candidate", 0.9, contained[0])

    scored: list[tuple[float, MPRecord]] = []
    for mp in candidates:
        ratio = SequenceMatcher(None, candidate_norm, mp.name_norm).ratio()
        if ratio >= 0.82:
            scored.append((ratio, mp))
    scored.sort(key=lambda item: item[0], reverse=True)
    if scored:
        best_ratio, best_mp = scored[0]
        second = scored[1][0] if len(scored) > 1 else 0.0
        if best_ratio >= 0.9 and best_ratio - second >= 0.03:
            return MatchResult("mp", f"fuzzy_period_roster:{best_ratio:.2f}", 0.88, best_mp)
        if best_ratio >= 0.84 and best_ratio - second >= 0.08:
            return MatchResult("mp", f"fuzzy_period_roster:{best_ratio:.2f}", 0.78, best_mp)

    if candidate_is_office(candidate):
        return MatchResult("office_or_role", "office_or_role_not_mp", 0.5)
    return MatchResult("unmatched_candidate", "no_period_roster_match", 0.0)


def split_paragraphs(path: Path) -> list[Paragraph]:
    paragraphs: list[Paragraph] = []
    lines: list[str] = []
    start_line: int | None = None
    last_line = 0

    def flush(end_line: int) -> None:
        nonlocal lines, start_line
        if lines and start_line is not None:
            text = "\n".join(lines).strip()
            if text:
                paragraphs.append(Paragraph(start_line, end_line, text))
        lines = []
        start_line = None

    with path.open(encoding="utf-8", errors="replace") as handle:
        for line_no, line in enumerate(handle, start=1):
            last_line = line_no
            if line.strip():
                if start_line is None:
                    start_line = line_no
                lines.append(line.rstrip())
            else:
                flush(line_no - 1)
    flush(last_line)
    return paragraphs


def is_noise_paragraph(paragraph: Paragraph) -> bool:
    text = SPACES.sub(" ", paragraph.text).strip()
    if not text:
        return True
    if all(NOISE_LINE.match(line.strip()) for line in paragraph.text.splitlines() if line.strip()):
        return True
    if len(text) < 3:
        return True
    if re.fullmatch(r"[\d\s/‑ـ-]+", text):
        return True
    return False


def is_substantive_paragraph(paragraph: Paragraph) -> bool:
    if is_noise_paragraph(paragraph):
        return False
    text = paragraph.text.strip()
    if len(re.findall(r"\S+", text)) < 3 and not SPEECH_OPENERS.search(text):
        return False
    return True


def find_last_handoff_cue(text: str) -> Cue | None:
    matches: list[tuple[int, re.Match[str], str]] = []
    for pattern in HANDOFF_PATTERNS:
        for match in pattern.finditer(text):
            matches.append((match.start(), match, "handoff_window"))
    if not matches:
        return None
    # Prefer the last usable cue, but skip self-referential imperative tails like
    # "نقطة نظام تفضلي" that do not name a speaker.
    for _, match, method in sorted(matches, key=lambda item: item[0], reverse=True):
        raw = match.group("name")
        clean = clean_candidate(raw)
        clean_norm = strip_honorifics(clean)
        if not clean or clean_norm in {"تفضلي", "تفضل", "فليتفضل", "فلتتفضل"}:
            continue
        if clean_norm.startswith(("من يدافع", "من يريد الدفاع", "من سيتولي الدفاع")):
            continue
        if len(clean_norm) < 2:
            continue
        return Cue(raw_name=raw.strip(), clean_name=clean, start=match.start(), end=match.end(), method=method)
    return None


def looks_like_speech_start(text: str) -> bool:
    text = SPACES.sub(" ", text).strip()
    if not text:
        return False
    if SPEECH_OPENERS.search(text):
        return True
    if re.match(r"^(?:السيد|السيدة|النائب|النائبة)\s+[^:،.؛]{2,60}:", text):
        return True
    if len(re.findall(r"\S+", text)) >= 10 and not is_boundary_text(text):
        return True
    return False


def is_boundary_text(text: str) -> bool:
    text = SPACES.sub(" ", text).strip()
    if not text:
        return True
    if SECTION_HEADER.search(text):
        return True
    if REPORT_HEADER.search(text[:250]):
        return True
    if VOTE_OR_ARTICLE.search(text[:250]):
        return True
    if find_last_handoff_cue(text):
        return True
    return False


def block_type_for_text(text: str) -> str:
    compact = SPACES.sub(" ", text).strip()
    if REPORT_HEADER.search(compact[:350]):
        return "committee_report_or_rapporteur"
    if VOTE_OR_ARTICLE.search(compact[:300]):
        return "vote_or_article"
    if SPEECH_OPENERS.search(compact):
        return "speech"
    return "speech_like"


def extract_turns(path: Path, meta: FileMeta, roster: dict[str, list[MPRecord]]) -> list[dict[str, object]]:
    period = legislature_period_for_file(meta)
    paragraphs = split_paragraphs(path)
    turns: list[dict[str, object]] = []
    consumed: set[int] = set()

    for i, paragraph in enumerate(paragraphs):
        if i in consumed:
            continue
        cue = find_last_handoff_cue(paragraph.text)
        if cue is None:
            continue

        match = link_candidate(cue.clean_name, period, roster)
        speech_parts: list[str] = []
        start_line: int | None = None
        end_line: int | None = None
        window_strategy = "next_substantive_paragraph"

        tail = paragraph.text[cue.end :].strip(" ،.؛:-\n")
        if looks_like_speech_start(tail):
            speech_parts.append(tail)
            start_line = paragraph.start_line
            end_line = paragraph.end_line
            consumed.add(i)
            j = i + 1
            window_strategy = "same_paragraph_tail"
        else:
            j = i + 1
            while j < len(paragraphs) and not is_substantive_paragraph(paragraphs[j]):
                consumed.add(j)
                j += 1
            if j < len(paragraphs) and not is_boundary_text(paragraphs[j].text):
                speech_parts.append(paragraphs[j].text)
                start_line = paragraphs[j].start_line
                end_line = paragraphs[j].end_line
                consumed.add(j)
                j += 1

        while speech_parts and j < len(paragraphs):
            next_para = paragraphs[j]
            if is_boundary_text(next_para.text):
                break
            if is_substantive_paragraph(next_para):
                speech_parts.append(next_para.text)
                end_line = next_para.end_line
                consumed.add(j)
            j += 1

        if not speech_parts:
            continue

        text = "\n\n".join(speech_parts).strip()
        idx = len(turns) + 1
        mp = match.mp
        turns.append(
            {
                "speech_id": f"{meta.document_id}__win_{idx:04d}",
                "source_file": meta.source_file,
                "document_id": meta.document_id,
                "speech_index": idx,
                "year_start": meta.year_start or "",
                "year_end": meta.year_end or "",
                "inferred_date": meta.inferred_date or "",
                "legislature_period": period,
                "start_line": start_line or "",
                "end_line": end_line or "",
                "speaker_target_raw": cue.raw_name,
                "speaker_target_clean": cue.clean_name,
                "speaker_type": match.speaker_type,
                "mp_id": mp.mp_id if mp else "",
                "mp_name_ar": mp.name_ar if mp else "",
                "mp_name_transliterated": mp.name_transliterated if mp else "",
                "mp_party_ar": mp.party_ar if mp else "",
                "mp_district_ar": mp.district_ar if mp else "",
                "assignment_method": f"{cue.method}:{match.method}",
                "assignment_confidence": f"{match.confidence:.2f}",
                "needs_review": "" if match.speaker_type == "mp" and match.confidence >= 0.78 else "1",
                "evidence_line": paragraph.start_line,
                "evidence_text": paragraph.text,
                "window_strategy": window_strategy,
                "block_type": block_type_for_text(text),
                "char_count": len(text),
                "word_count": len(re.findall(r"\S+", text)),
                "text": text,
            }
        )

    return turns


FIELDS = [
    "speech_id",
    "source_file",
    "document_id",
    "speech_index",
    "year_start",
    "year_end",
    "inferred_date",
    "legislature_period",
    "start_line",
    "end_line",
    "speaker_target_raw",
    "speaker_target_clean",
    "speaker_type",
    "mp_id",
    "mp_name_ar",
    "mp_name_transliterated",
    "mp_party_ar",
    "mp_district_ar",
    "assignment_method",
    "assignment_confidence",
    "needs_review",
    "evidence_line",
    "evidence_text",
    "window_strategy",
    "block_type",
    "char_count",
    "word_count",
    "text",
]


def write_csv(path: Path, rows: Iterable[dict[str, object]]) -> None:
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def in_scope_files() -> list[Path]:
    paths = []
    for path in ROOT.glob("*_cleaned_clean.md"):
        meta = infer_file_meta(path)
        if meta.include:
            paths.append(path)
    return sorted(paths)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sample-size", type=int, default=10)
    parser.add_argument("--seed", type=int, default=20260702)
    parser.add_argument("--all", action="store_true")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUT)
    args = parser.parse_args()

    files = in_scope_files()
    rng = random.Random(args.seed)
    if args.all:
        selected = files
        candidate_files = selected
    else:
        candidate_files = files[:]
        rng.shuffle(candidate_files)
        selected = []

    roster = read_roster(ROSTER_PATH)
    all_rows: list[dict[str, object]] = []
    manifest: list[dict[str, object]] = []

    for path in candidate_files:
        if not args.all and len(selected) >= args.sample_size:
            break
        meta = infer_file_meta(path)
        try:
            rows = extract_turns(path, meta, roster)
        except OSError as exc:
            print(f"warning: skipping {path.name}: {exc}", file=sys.stderr)
            manifest.append(
                {
                    "source_file": path.name,
                    "rows": 0,
                    "year_start": meta.year_start,
                    "year_end": meta.year_end,
                    "period": legislature_period_for_file(meta),
                    "status": f"skipped: {exc}",
                }
            )
            continue
        selected.append(path)
        all_rows.extend(rows)
        manifest.append(
            {
                "source_file": path.name,
                "rows": len(rows),
                "year_start": meta.year_start,
                "year_end": meta.year_end,
                "period": legislature_period_for_file(meta),
                "status": "processed",
            }
        )

    output = args.output if args.output.is_absolute() else ROOT / args.output
    write_csv(output, all_rows)

    print(f"selected_files={len(selected)}")
    print(f"rows={len(all_rows)}")
    print(f"output={output}")
    print("manifest=" + json.dumps(manifest, ensure_ascii=False))


if __name__ == "__main__":
    main()
