#!/usr/bin/env python3
"""Cue-window speaker assignment for Tunisian parliamentary minutes.

This is a test implementation of a handoff-window strategy. It reads the
pre-speaker-assignment minutes in `TN_democratic_cleaned_clean_md/`, detects
explicit chair handoff cues, extracts the named candidate, and captures the
speech window following that cue.

All paths are repo-relative, so the script runs on any clone of MENA_PERC.
Overridable via environment variables:
    TUNISIA_CORPUS_DIR   corpus directory (default: <repo>/TN_democratic_cleaned_clean_md)
    TUNISIA_MP_ROSTER    roster CSV (default: <repo>/data/Tunisian Parliamentary Dataset.csv)
    TUNISIA_ALIAS_TABLE  alias table CSV (default: next to this script)
    TUNISIA_YEAR_MIN / TUNISIA_YEAR_MAX   year scope (default: 2010-2025)

Default run mode samples 10 in-scope files and writes `test_corpus.csv`.
Use `--all` to process all in-scope files, or `--sample-size N`.

Data-driven alias tier (no hardcoded ALIAS_RAW):
    python3 handoff_window_assignment.py --build-aliases   # writes alias_table.csv
The linker loads alias_table.csv if present; it maps a normalized cue-name
variant to a canonical mp_id, mined from the corpus's own consistent
resolutions (see build_alias_table()).  Regenerate it whenever the corpus
changes; hand-review rows before trusting them for publication.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import random
import re
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass
from difflib import SequenceMatcher
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT = ROOT / "test_corpus.csv"
CORPUS_DIR = Path(os.environ.get("TUNISIA_CORPUS_DIR", str(ROOT / "TN_democratic_cleaned_clean_md")))
ALIAS_PATH = Path(os.environ.get("TUNISIA_ALIAS_TABLE", str(Path(__file__).resolve().parent / "alias_table.csv")))
ROSTER_PATH = Path(
    os.environ.get(
        "TUNISIA_MP_ROSTER",
        str(ROOT / "data" / "Tunisian Parliamentary Dataset.csv"),
    )
)

# default scope covers the full repo corpus (2010-2025); narrow via env,
# e.g. TUNISIA_YEAR_MIN=2011 TUNISIA_YEAR_MAX=2021 — files outside roster
# periods still yield turns with role/raw-name attribution.
YEAR_MIN = int(os.environ.get("TUNISIA_YEAR_MIN", "2010"))
YEAR_MAX = int(os.environ.get("TUNISIA_YEAR_MAX", "2025"))

# ---------------------------------------------------------------------------
# Fuzzy-link thresholds.  These are not magic: they were validated against the
# 9,669 independently chair-cue-confirmed (name -> mp_id) pairs in the
# deterministic reference set.  Because linking is PERIOD-SCOPED (only the
# ~200 MPs sitting in that legislature are candidates), a lower floor is safe:
# at FUZZY_STRONG the measured precision is ~0.998 with the uniqueness margin.
# Raise FUZZY_FLOOR toward 0.90 for fewer/higher-certainty names.
FUZZY_STRONG = 0.90     # accept with a small uniqueness margin
FUZZY_STRONG_MARGIN = 0.03
FUZZY_FLOOR = 0.84      # accept only with a large uniqueness margin
FUZZY_FLOOR_MARGIN = 0.08

LEADING_DATE = re.compile(r"^(?P<year>20\d{2}|19\d{2})[-_](?P<month>\d{1,2})[-_](?P<day>\d{1,2})")
LEADING_COMPACT_DATE = re.compile(r"^(?P<year>20\d{2}|19\d{2})(?P<month>\d{2})(?P<day>\d{2})")
LEADING_SESSION_RANGE = re.compile(r"^(?P<start>20\d{2})-(?P<end>20\d{2})_")
MUD_AWALAT_2011 = re.compile(r"^2011_Mudawalat_2011_")
BULLETIN_DATE = re.compile(
    r"bulletin_officiel_.*?[-_](?P<day>\d{1,2})[-_](?P<month>\d{1,2})[-_](?P<year>20\d{2})",
    re.I,
)

AR_DIACRITICS = re.compile(r"[ً-ٰٟـ]")
NON_AR_NUM_SPACE = re.compile(r"[^؀-ۿ0-9 ]+")
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
    "المحترم",
    "المحترمه",
    "محترم",
    "محترمه",
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

NO_AR_LETTER_AFTER = r"(?![؀-ۿ])"
TFADEL = r"تفض[ً-ٰٟـ]*ل" + NO_AR_LETTER_AFTER
TFADELI = r"تفض[ً-ٰٟـ]*لي" + NO_AR_LETTER_AFTER
FALYATAFADEL = r"فليتفض[ً-ٰٟـ]*ل" + NO_AR_LETTER_AFTER
FALTATAFADEL = r"فلتتفض[ً-ٰٟـ]*ل" + NO_AR_LETTER_AFTER
TFADEL_WORDS = rf"(?:{TFADEL}|{TFADELI}|{FALYATAFADEL}|{FALTATAFADEL})"

# Honorific building blocks shared by the cue patterns.  HONOR_ANY includes
# bare (no ال) and feminine/OCR variants; LIL_HONOR is the لل-prefixed set.
HONOR_ANY = (
    r"(?:السيدة|السيده|السيد|النائبة|النائبه|النائب|الزميلة|الزميله|الزميل|"
    r"الأستاذة|الأستاذ|الاستاذة|الاستاذه|الاستاذ|الدكتورة|الدكتوره|الدكتور|"
    r"سيدتي|سيدي|سيدة|سيده|سيد|نائبة|نائب)"
)
LIL_HONOR = (
    r"(?:للسيدة|للسيده|للسيد|للنائبة|للنائبه|للنائب|للزميلة|للزميله|للزميل|"
    r"للأستاذة|للأستاذ|للاستاذة|للاستاذه|للاستاذ|للدكتورة|للدكتوره|للدكتور)"
)
# Chair/self-address guard: a شكرا cue naming الرئيس/الوزير is an MP addressing
# the chair or a minister at the START of their own speech, not a handoff.
NOT_CHAIR_OR_GOV = r"(?!الرئيس|الرئيسة|الرئيسه|رئيس|رئيسة|رئيسه|الوزير|الوزيرة|الوزيره)"
NUM_WORD = r"(?:خمس|ثلاث|أربع|اربع|ست|سبع|ثماني|تسع|عشر|إحدى|احدى|اثنتا|نصف)"
DURATION_PHRASE = rf"(?:(?:له|لها|لك|لديك|لديها)\s+)?(?:{NUM_WORD}\s+)?دق[ياـئ]?[قئ][هةا]?\S*"
# Strict variant for p8: a possessive or a numeral is REQUIRED before the
# minutes word.  Without it, p8 fired on bare مدة/دقيقة mentions in roll-call
# announcements (manual review: rejected event, 2011 Tome 1 line 102).
DURATION_STRICT = rf"(?:(?:له|لها|لك|لديك|لديها)\s+(?:{NUM_WORD}\s+)?|{NUM_WORD}\s+)دق[ياـئ]?[قئ][هةا]?\S*"
# يتولى/تتولى الدفاع family (defense assignment verbs, both ى/ي spellings)
TAWALLA = r"(?:و?[يت]تول[ىي]|و?س[يت]تول[ىي])"

HANDOFF_PATTERNS = [
    # p0: (verb +) الكلمة (+ الآن) (+ إلى) (+ لل-honorific) NAME  — incl. الكلمه OCR variant
    re.compile(
        r"الكلم[ةه]\s+"
        r"(?:الآن\s+)?(?:إلى\s+|الى\s+)?"
        rf"(?:{LIL_HONOR}|ل)?\s*(?P<name>[^،.؛:\n]+)"
    ),
    # p1: نقطة نظام (+ إلى/لل) NAME
    re.compile(
        r"(?:نقطة نظام)\s+(?:إلى\s+|الى\s+)?"
        rf"(?:{LIL_HONOR}|ل)?\s*"
        r"(?P<name>[^،.؛:\n]+)"
    ),
    # p2: تفضل/تفضلي (+ يا) (+ honorific) NAME
    re.compile(
        TFADEL_WORDS + r"\s+(?:يا\s+)?"
        rf"{HONOR_ANY}?\s*"
        r"(?P<name>[^،.؛:\n]+)"
    ),
    # p3: honorific NAME (,) تفضل — comma between name and تفضل tolerated
    re.compile(
        rf"{HONOR_ANY}\s+"
        r"(?P<name>[^،.؛:\n]+?)\s*[،,]?\s+" + TFADEL_WORDS
    ),
    # p4: شكرا، (لل)honorific NAME (duration)? at end of line — thanks-and-call.
    #     The comma right after شكرا distinguishes "thanks; NAME (your turn)"
    #     from "thanks TO NAME" (previous speaker, no comma).
    re.compile(
        rf"شكر(?:ًا|اً|ا|ً)?\s*(?:جزيلاً|جزيلا)?\s*[،,]\s*(?:لل)?{HONOR_ANY}{NOT_CHAIR_OR_GOV}\s+"
        rf"(?P<name>[^،.؛:\n]{{2,60}}?)\s*(?:[،,]?\s*{DURATION_PHRASE})?\s*[.؛]?\s*$",
        re.M,
    ),
    # p5: يتفضل/ستتفضل/وستفضل/ليتفضل + (honorific) NAME — verb-first handoff
    re.compile(
        r"(?:و?س?[يت]تفض[ً-ٰٟـ]*ل|وستفض[ً-ٰٟـ]*ل|فل[يت]تفض[ً-ٰٟـ]*ل|ل[يت]تفض[ً-ٰٟـ]*ل)\s+"
        rf"(?:الآن\s+)?(?:{HONOR_ANY}\s+)?(?P<name>[^،.؛:\n]+)"
    ),
    # p6: نستمع (الآن) إلى/لل + honorific NAME — honorific REQUIRED, otherwise
    # mid-speech "لا نستمع إلى بعضنا" produces junk candidates
    re.compile(
        rf"(?:س?نستمع)\s+(?:الآن\s+)?(?:إلى\s+|الى\s+|الي\s+)?(?:{LIL_HONOR}|{HONOR_ANY})\s*(?P<name>[^،.؛:\n]+)"
    ),
    # p7: ننتقل/نمر (الآن) إلى + honorific NAME
    re.compile(
        r"(?:ننتقل|نمر|سنمر)\s+(?:الآن\s+)?(?:إلى|الى|الي)\s+"
        rf"(?:{HONOR_ANY}|{LIL_HONOR})\s*(?P<name>[^،.؛:\n]+)"
    ),
    # p8: honorific NAME (له/لها) N دقائق — time allocation implies the floor.
    # الرئيس blocked: "السيدة الرئيسة ... دقائق" is MPs talking ABOUT the chair.
    # STRICT duration (numeral/possessive required) per manual-review feedback.
    re.compile(
        rf"{HONOR_ANY}(?!الرئيس|الرئيسة|الرئيسه)\s+(?P<name>[^،.؛:\n]{{2,60}}?)\s*[،,]?\s+{DURATION_STRICT}"
    ),
    # p9: السؤال ... من قبل / موجه من / لل + NAME — oral-question turns
    re.compile(
        rf"(?:السؤال|سؤال)(?:\s+[^\s،.؛:]+){{0,4}}?\s+"
        rf"(?:(?:من\s+قبل|موجه\s+من|وهو\s+لل|طرحه)\s*{HONOR_ANY}?|{LIL_HONOR})\s*"
        r"(?P<name>[^،.؛:\n]+)"
    ),
    # p10: يدافع عن (المقترح) + honorific NAME — amendment defense
    re.compile(
        rf"يدافع\s+عن(?:\s+[^\s،.؛:]+){{0,4}}?\s+{HONOR_ANY}\s+(?P<name>[^،.؛:\n]+)"
    ),
    # p11: والرأي (ال)ضد/المخالف + honorific NAME — counter-speaker
    re.compile(
        rf"(?:و?الرأي)\s+(?:ال)?(?:ضد|مخالف|المخالف|المعارض)\s+(?:{HONOR_ANY}|{LIL_HONOR})\s*(?P<name>[^،.؛:\n]+)"
    ),
    # p12: تفضلوا السيد NAME — polite-plural handoff (ministers, officials)
    re.compile(
        rf"تفضلوا\s+{HONOR_ANY}\s+(?P<name>[^،.؛:\n]+)"
    ),
    # p13: يتولى/تتولى (…) الدفاع constructions, both word orders
    # (manual review 2015-11-19: "ويتولى الدفاع عنه الأستاذة فريدة عبيدي",
    #  "وتتولى الأستاذة سامية عبو الدفاع عن هذا المقترح")
    re.compile(
        rf"{TAWALLA}\s+الدفاع\s+عن\S*\s*(?:[^\s،.؛:]+\s+){{0,3}}?{HONOR_ANY}\s+(?P<name>[^،.؛:\n]+)"
    ),
    re.compile(
        rf"{TAWALLA}\s+{HONOR_ANY}\s+(?P<name>[^،.؛:\n]{{2,60}}?)\s+الدفاع"
    ),
    # p15: rhetorical question then bare name at line end —
    # "من له رأي معارض؟ السيدة سامية عبو."  The ؟ anchor keeps this out of
    # running speech.
    re.compile(
        rf"؟\s*{HONOR_ANY}\s+(?P<name>[^،.؛:\n]{{2,60}}?)\s*[.؛]?\s*$",
        re.M,
    ),
]

STOP_AFTER_CANDIDATE = re.compile(
    rf"\s+(?:{TFADEL}|{TFADELI}|تفضيلي|{FALYATAFADEL}|{FALTATAFADEL}|لديه|لديها|له|لها|لك|لديك|خمس|ثلاث|"
    r"أربع|اربع|ست|سبع|ثماني|تسع|عشر|إحدى|احدى|دقيقتان|دقيقتين|دقيقة|دقيقه|دقائق|غير موجود|في نقطة|نقطة نظام|"
    r"للدفاع|للدفاع عن|لتلاوة|لتقديم|لعرض|ليقدم|لتقدم|في حدود|ثم|لكن|قبل|بعد|نمر|نمرر|نواصل|"
    # verbs that follow the name in chair announcements (manual-review fixes):
    # "السيد X يطلب نقطة نظام" / "السيدة Y تتولى الدفاع" / "سيتولى الدفاع"
    r"يطلب|تطلب|يتولى|يتولي|تتولى|تتولي|سيتولى|سيتولي|ستتولى|ستتولي|$)"
)

SPEECH_OPENERS = re.compile(
    r"^(?:بسم الله|شكراً|شكرا|سيدي الرئيس|سيد الرئيس|السيدة الرئيسة|سيده الرئيسه|"
    r"حضرات|زملائي|زميلاتي|مرحبا|صباح الخير|يومكم سعيد)"
)

NOISE_LINE = re.compile(
    r"^(?:صفحة\s+\d+|\d{3,}|صد\s+\d+|مداولات مجلس|الدورة العادية|المدة النيابية|"
    r"##\s+|افتتاح الجلسة|استئناف الجلسة|رفع الجلسة)\s*$"
)

# NOTE: "الفصل" (article) citations are routinely glued to their number with no
# space by the extractor ("الفصل6", "الفصل02") — \s* (not \s+) so these still
# match; measured 7,657 of 9,211 "الفصل+digit" block-starts were glued.
SECTION_HEADER = re.compile(r"^(?:#{1,6}\s+|(?:الباب|القسم|الفرع|الفصل)\s*\S+)")
VOTE_OR_ARTICLE = re.compile(
    r"(?:نمر إلى التصويت|نمر للتصويت|التصويت|تمت الموافقة|يعرض مباشرة على التصويت|"
    r"الفصل\s*\d|مشروع قانون|مشروع الميزانية)"
)
# Roll-call vote tally announcement ("115 صوتا نعم مع احتفاظ5 أصوات ومعارضة10
# أصوات"): chair reading out a vote result is never part of an MP's speech and
# always ends the capture window. Requires digit+صوت+نعم together (not bare
# "النتيجة", which MPs use generically ~4,900 times in ordinary speech with no
# vote-count nearby — checked, only ~2,000 of those co-occur with a tally).
VOTE_RESULT = re.compile(r"\d+\s*صوتا?\s*[\"”]?\s*(?:ب)?نعم")
# Boundary version: START-anchored chair formulas only.  The old behaviour
# (searching text[:250] with VOTE_OR_ARTICLE) marked any MP speech that merely
# MENTIONS التصويت or "الفصل N" in its opening lines as a boundary, killing the
# capture window (~100 lost speech-opener paragraphs per 60 files, measured).
VOTE_BOUNDARY = re.compile(
    r"^\s*(?:نمر\s+(?:الآن\s+)?(?:إلى|الى|الي)\s+التصويت|نمر\s+للتصويت|نصوت|"
    r"أعرض\s+على\s+التصويت|يعرض\s+(?:مباشرة\s+)?على\s+التصويت|التصويت\s+على|"
    r"تمت\s+الموافقة|مقترح\s+التعديل|الفصل\s*\d)"
)
# Session-closing formulas: "رفعت الجلسة على الساعة ..." / "(كانت الساعة ...)" /
# "اختتمت الجلسة".  Paragraph-start anchored so a speech that merely RECALLS a
# past adjournment mid-sentence never becomes a boundary.  These end the capture
# window: nothing after the adjournment line belongs to the running speech
# (what follows is back-matter or the next sitting).
ADJOURN_BOUNDARY = re.compile(
    r"^\s*\(?\s*(?:و?رفعت\s+الجلسة|و?كانت\s+الساعة|اختتمت\s+الجلسة|"
    r"وبهذا\s+نختتم|رفعت\s+الجلسه)"
)
# Session-resumption chair formula ("تستأنف/نستأنف الجلسة..."): always a fresh
# procedural cue, never the tail of the PREVIOUS speech window. Measured: 63
# hits corpus-wide, median block length 61 chars (short chair announcements,
# not substantive speech content).
RESUME_BOUNDARY = re.compile(r"^\s*(?:است[ةأ]?نفت|تستأنف|نستأنف)\s+الجلسة")
# Same adjournment formulas as ADJOURN_BOUNDARY but un-anchored: catches a
# chair closing their OWN remarks with "...ونرفع الجلسة (كانت الساعة ...)" in
# the middle of what is otherwise a real captured paragraph. Used to truncate
# a paragraph's text at the adjournment point and hard-stop window growth,
# instead of only blocking whichever paragraph happens to start with it.
# Measured: 214/536 adjournment-containing turns had >300 chars of trailing
# text incorrectly captured after the sitting had already closed.
ADJOURN_ANYWHERE = re.compile(
    r"\(?\s*(?:و?رفعت\s+الجلسة|و?كانت\s+الساعة|اختتمت\s+الجلسة|وبهذا\s+نختتم|رفعت\s+الجلسه)"
)

def truncate_at_adjournment(text: str) -> tuple[str, bool]:
    m = ADJOURN_ANYWHERE.search(text)
    if not m:
        return text, False
    nl = text.find("\n", m.end())
    cut = nl if nl != -1 else len(text)
    return text[:cut].rstrip(), True
# A cue paragraph cut by a PAGE BREAK ends in a dangling connector: stitch it
# to the next real paragraph before cue detection.
DANGLING_END = re.compile(
    rf"(?:الكلم[ةه]|{LIL_HONOR}|{HONOR_ANY}|إلى|الى|الي|ثم|و)\s*$"
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
    document_id = name.removesuffix(".md").removesuffix("_cleaned_clean")

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
    reason = "in_scope_year" if include else f"outside_{YEAR_MIN}_{YEAR_MAX}_or_unknown_date"
    return FileMeta(name, document_id, year_start, year_end, inferred_date, include, reason)


def legislature_period_for_file(meta: FileMeta) -> str:
    """Map a file to its legislature by DATE (roster period keys).

    Chamber of Deputies 2009-10-25..2011-01  -> "2009-2011"
    NCA (التأسيسي)      2011-11-22..2014-10  -> "2011-2014"
    ARP I               2014-12-02..2019-11  -> "2014-2019"
    ARP II              2019-11-13..2021-07 (suspended) -> "2019-2024"
    ARP III             2023-03-13..         -> "2023-2027"
    Session-range files (e.g. 2019-2020_x) belong to the legislature whose
    term contains that parliamentary year (Oct year_start .. Jul year_end).
    """
    name = meta.source_file
    if name.startswith("2011_Mudawalat") or name.startswith("2011-2014"):
        return "2011-2014"
    ys = meta.year_start
    if not ys:
        return ""
    d = meta.inferred_date or ""
    if d:  # exact sitting date known
        if d < "2011-02-01":
            return "2009-2011"
        if d < "2014-11-15":
            return "2011-2014"
        if d < "2019-11-13":
            return "2014-2019"
        if d < "2022-06-01":
            return "2019-2024"
        return "2023-2027"
    # session-range only: parliamentary year Oct ys .. Jul year_end
    if ys <= 2010:
        return "2009-2011"
    if ys <= 2013:
        return "2011-2014"
    if ys <= 2018:
        return "2014-2019"
    if ys <= 2021:
        return "2019-2024"
    return "2023-2027"


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


# ---------------------------------------------------------------------------
# Data-driven alias table (replaces any hand-typed ALIAS_RAW).
# alias_table.csv columns: variant_norm, mp_id, roster_name, n_occurrences, source
_ALIASES: dict[str, str] = {}


def load_aliases(path: Path = ALIAS_PATH) -> dict[str, str]:
    global _ALIASES
    _ALIASES = {}
    if path.exists():
        with path.open(encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                v = (row.get("variant_norm") or "").strip()
                m = (row.get("mp_id") or "").strip()
                if v and m:
                    _ALIASES[v] = m
    return _ALIASES


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

    # (1) exact within-period roster name
    exact = [mp for mp in candidates if mp.name_norm == candidate_norm]
    if len(exact) == 1:
        conf = 1.0 if candidate_norm == normalize_arabic(candidate) else 0.95
        return MatchResult("mp", "exact_period_roster", conf, exact[0])

    # (2) roster name fully contained in the candidate string
    contained = [mp for mp in candidates if mp.name_norm and mp.name_norm in candidate_norm]
    if len(contained) == 1:
        return MatchResult("mp", "roster_name_contained_in_candidate", 0.9, contained[0])

    # (2b) the reverse direction: every candidate token inside a roster name, in
    #      order (the chair drops middle names: 'سامية عبو' -> roster
    #      'سامية حمودة عبو'). >=2 tokens and within-period uniqueness required.
    ctoks = candidate_norm.split()
    if len(ctoks) >= 2:
        def _subseq(small: list[str], big: list[str]) -> bool:
            it = iter(big)
            return all(t in it for t in small)
        sub = [mp for mp in candidates
               if mp.name_norm and _subseq(ctoks, mp.name_norm.split())]
        if len(sub) == 1:
            return MatchResult("mp", "candidate_tokens_in_roster_name", 0.9, sub[0])

    # (3) DATA-DRIVEN alias (mined from consistent resolutions; NOT hand-typed).
    #     Catches hard OCR variants that no safe fuzzy threshold can reach
    #     (e.g. 'فيصل تبني' -> 'فيصل التبيني'). Period-checked: the aliased mp_id
    #     must actually sit in this legislature.
    alias_mid = _ALIASES.get(candidate_norm)
    if alias_mid:
        hit = [mp for mp in candidates if mp.mp_id == alias_mid]
        if len(hit) == 1:
            return MatchResult("mp", "alias_table", 0.9, hit[0])

    # (4) bounded fuzzy within the period roster (thresholds validated; see header)
    scored: list[tuple[float, MPRecord]] = []
    for mp in candidates:
        ratio = SequenceMatcher(None, candidate_norm, mp.name_norm).ratio()
        if ratio >= 0.82:
            scored.append((ratio, mp))
    scored.sort(key=lambda item: item[0], reverse=True)
    if scored:
        best_ratio, best_mp = scored[0]
        second = scored[1][0] if len(scored) > 1 else 0.0
        if best_ratio >= FUZZY_STRONG and best_ratio - second >= FUZZY_STRONG_MARGIN:
            return MatchResult("mp", f"fuzzy_period_roster:{best_ratio:.2f}", 0.88, best_mp)
        if best_ratio >= FUZZY_FLOOR and best_ratio - second >= FUZZY_FLOOR_MARGIN:
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


def stitch_paragraphs(paragraphs: list[Paragraph]) -> list[Paragraph]:
    """Repair page-break damage before cue detection.

    When a handoff cue sits at the bottom of a PDF page, the page footer /
    number / running header splits it from its continuation ("...الكلمة للسيد"
    <page junk> "فلان الفلاني").  Two deterministic repairs:
      1. drop pure-noise paragraphs (page numbers, running headers) so they
         never separate a cue from its speech window;
      2. if a paragraph ends in a dangling connector (الكلمة / للسيد / إلى /
         honorific / ثم / و), merge it with the next surviving paragraph.
    """
    kept = [p for p in paragraphs if not is_noise_paragraph(p)]
    out: list[Paragraph] = []
    i = 0
    while i < len(kept):
        para = kept[i]
        while i + 1 < len(kept) and DANGLING_END.search(para.text.strip()):
            nxt = kept[i + 1]
            para = Paragraph(para.start_line, nxt.end_line, para.text.rstrip() + " " + nxt.text.lstrip())
            i += 1
        out.append(para)
        i += 1
    return out


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
    for pat_idx, pattern in enumerate(HANDOFF_PATTERNS):
        for match in pattern.finditer(text):
            matches.append((match.start(), match, f"handoff_window:p{pat_idx}"))
    if not matches:
        return None
    # Prefer the LAST usable cue: in "thanks Sam ... the floor goes to John"
    # paragraphs the recipient (John) is named last, so last-cue-wins resolves
    # giver/recipient correctly.  Skip self-referential imperative tails like
    # "نقطة نظام تفضلي" that do not name a speaker.
    for _, match, method in sorted(matches, key=lambda item: item[0], reverse=True):
        raw = match.group("name")
        clean = clean_candidate(raw)
        clean_norm = strip_honorifics(clean)
        if not clean or clean_norm in {"تفضلي", "تفضل", "تفضلوا", "فليتفضل", "فلتتفضل"}:
            continue
        if clean_norm.startswith(("من يدافع", "من يريد الدفاع", "من سيتولي الدفاع")):
            continue
        if len(clean_norm) < 2:
            continue
        # A real name (even with a role title) is short; long tails are cue
        # verbs firing inside running speech, not handoffs.
        if len(clean_norm.split()) > 7:
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
    if VOTE_BOUNDARY.match(text):
        return True
    if ADJOURN_BOUNDARY.match(text):
        return True
    if RESUME_BOUNDARY.match(text):
        return True
    if VOTE_RESULT.search(text):
        return True
    if find_last_handoff_cue(text):
        return True
    return False


# Procedural = short, cue-dominated chair floor-management / voting mechanics /
# quorum (attendance counts, hand-raising, adjournment proposals). Not covered by
# VOTE_OR_ARTICLE, which tags bill/vote *text*. Word-count guarded so a real speech
# that merely mentions a vote is never mislabelled (preserves recall — no boundary
# change, this only refines the block_type tag).
PROC_CUE = re.compile(
    r"عدد\s+الحاضرين|عدد\s+الحضور|هل\s+من\s+حضور|بتسجيل\s+الحضور|النصاب|"
    r"رفع\s+الأيدي|رفع\s+الايدي|من\s+يوافق|من\s+يعارض|من\s+يحتفظ(?:\s+بصوت)?|"
    r"بالإجماع|بالاجماع|المصادقة\s+على|هل\s+هناك\s+(?:ملاحظات?|اعتراض)|"
    r"أعلن\s+رفع\s+الجلسة|أقترح\s+رفع\s+الجلسة")


def is_procedural_block(text: str) -> bool:
    compact = SPACES.sub(" ", text).strip()
    wc = len(re.findall(r"\S+", compact))
    hits = len(PROC_CUE.findall(compact))
    return (hits >= 1 and wc <= 30) or (hits >= 3 and wc <= 60)


def block_type_for_text(text: str) -> str:
    compact = SPACES.sub(" ", text).strip()
    if REPORT_HEADER.search(compact[:350]):
        return "committee_report_or_rapporteur"
    if VOTE_OR_ARTICLE.search(compact[:300]):
        return "vote_or_article"
    if SPEECH_OPENERS.search(compact):
        return "speech"
    if is_procedural_block(compact):
        return "procedural"
    return "speech_like"


def extract_turns(path: Path, meta: FileMeta, roster: dict[str, list[MPRecord]]) -> list[dict[str, object]]:
    period = legislature_period_for_file(meta)
    paragraphs = stitch_paragraphs(split_paragraphs(path))
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

        adjourned = False
        tail = paragraph.text[cue.end :].strip(" ،.؛:-\n")
        if looks_like_speech_start(tail):
            tail, adjourned = truncate_at_adjournment(tail)
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
                ptext, adjourned = truncate_at_adjournment(paragraphs[j].text)
                speech_parts.append(ptext)
                start_line = paragraphs[j].start_line
                end_line = paragraphs[j].end_line
                consumed.add(j)
                j += 1

        while not adjourned and speech_parts and j < len(paragraphs):
            next_para = paragraphs[j]
            if is_boundary_text(next_para.text):
                break
            if is_substantive_paragraph(next_para):
                ptext, adjourned = truncate_at_adjournment(next_para.text)
                speech_parts.append(ptext)
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
                # For function-introduced speeches (وزير التجارة، رئيس اللجنة،
                # المقرر...) keep the verbatim role string so a downstream LLM
                # pass can decode role -> person (session date + gov roster).
                "speaker_role": cue.clean_name if match.speaker_type == "office_or_role" else "",
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
    "speaker_role",
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
    if not CORPUS_DIR.is_dir():
        sys.exit(f"corpus directory not found: {CORPUS_DIR}")
    paths = []
    for path in CORPUS_DIR.rglob("*.md"):
        meta = infer_file_meta(path)
        if meta.include:
            paths.append(path)
    return sorted(paths)


def build_alias_table(roster: dict[str, list[MPRecord]], out: Path = ALIAS_PATH) -> int:
    """Mine a data-driven variant->mp_id alias table from the corpus's own
    high-confidence resolutions — no hand-typed entries.

    Method (deterministic):
      1. Run cue extraction over ALL in-scope files with the current linker.
      2. For every cue, record (norm cue-name -> resolved mp_id) ONLY when the
         match is high-confidence (exact / contained / strong-fuzzy).  These are
         the "anchors": a normalized string that the corpus itself resolves the
         same way many times.
      3. For each UNRESOLVED cue name, attach it to an anchor MP iff the
         unresolved string shares a DISTINCTIVE surname token (a token owned by
         exactly one anchored MP within that period) — this is what catches
         'فيصل تبني' next to the anchored 'فيصل التبيني'.  A variant is written
         only if it maps to a single MP with no competing candidate.
    The table is advisory: review before publication.
    """
    files = in_scope_files()
    anchor_votes: dict[str, Counter] = defaultdict(Counter)      # norm_name -> Counter(mp_id)
    unresolved: Counter = Counter()                               # norm_name -> count
    period_of_name: dict[str, str] = {}
    # distinctive-token index per period: token -> set(mp_id) among ANCHORED MPs
    for path in files:
        meta = infer_file_meta(path)
        period = legislature_period_for_file(meta)
        for para in stitch_paragraphs(split_paragraphs(path)):
            cue = find_last_handoff_cue(para.text)
            if cue is None:
                continue
            nn = strip_honorifics(cue.clean_name)
            if not nn or len(nn.split()) < 2 or candidate_is_office(cue.clean_name):
                continue
            m = link_candidate(cue.clean_name, period, roster)
            if m.speaker_type == "mp" and m.mp and m.confidence >= 0.88:
                anchor_votes[nn][m.mp.mp_id] += 1
                period_of_name.setdefault(nn, period)
            elif m.speaker_type in ("unmatched_candidate", "office_or_role"):
                unresolved[nn] += 1
                period_of_name.setdefault(nn, period)

    # anchored names: unambiguous mp_id
    anchored = {nn: c.most_common(1)[0][0] for nn, c in anchor_votes.items()
                if len(c) == 1 or c.most_common(1)[0][1] >= 3 * (c.most_common(2)[1][1] if len(c) > 1 else 0 or 1)}
    # per-period distinctive surname token -> mp_id (token owned by one anchored MP)
    tok_owner: dict[tuple[str, str], set] = defaultdict(set)
    id_name = {}
    for period, recs in roster.items():
        for mp in recs:
            id_name[mp.mp_id] = mp.name_ar
    for nn, mid in anchored.items():
        per = period_of_name.get(nn, "")
        for t in strip_honorifics(id_name.get(mid, nn)).split():
            if len(t) >= 4:
                tok_owner[(per, t)].add(mid)

    rows = []
    seen = set()
    # (a) anchors themselves that aren't exact roster names -> record as aliases
    for nn, mid in anchored.items():
        rows.append((nn, mid, id_name.get(mid, ""), anchor_votes[nn][mid], "anchor"))
        seen.add(nn)
    # (b) unresolved variants attached via a distinctive shared token
    for nn, cnt in unresolved.items():
        if nn in seen:
            continue
        per = period_of_name.get(nn, "")
        owners = set()
        for t in nn.split():
            if len(t) >= 4 and len(tok_owner.get((per, t), set())) == 1:
                owners |= tok_owner[(per, t)]
        if len(owners) == 1:
            mid = next(iter(owners))
            rows.append((nn, mid, id_name.get(mid, ""), cnt, "distinctive_token"))
            seen.add(nn)

    with out.open("w", encoding="utf-8", newline="") as handle:
        w = csv.writer(handle)
        w.writerow(["variant_norm", "mp_id", "roster_name", "n_occurrences", "source"])
        for r in sorted(rows, key=lambda x: -x[3]):
            w.writerow(r)
    print(f"alias table: {len(rows)} entries "
          f"({sum(1 for r in rows if r[4]=='anchor')} anchors, "
          f"{sum(1 for r in rows if r[4]=='distinctive_token')} token-attached) -> {out}")
    return len(rows)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sample-size", type=int, default=10)
    parser.add_argument("--seed", type=int, default=20260702)
    parser.add_argument("--all", action="store_true")
    parser.add_argument("--build-aliases", action="store_true",
                        help="mine alias_table.csv from the corpus, then exit")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUT)
    args = parser.parse_args()

    roster = read_roster(ROSTER_PATH)

    if args.build_aliases:
        build_alias_table(roster)
        return

    load_aliases()  # advisory data-driven aliases, if present

    files = in_scope_files()
    rng = random.Random(args.seed)
    if args.all:
        selected = []
        candidate_files = files
    else:
        candidate_files = files[:]
        rng.shuffle(candidate_files)
        selected = []

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
    print(f"aliases_loaded={len(_ALIASES)}")
    print(f"output={output}")
    print("manifest=" + json.dumps(manifest, ensure_ascii=False))


if __name__ == "__main__":
    main()
