#!/usr/bin/env python3
"""Validate and repair Tunisian sitting dates for the MENA_PERC democratic corpus.

Companion to ``handoff_window_assignment.py``: that script assigns speakers,
this one cross-checks the sitting-date index both rely on
(``Python scripts/manifest.csv``, one row per corpus document) and writes a
validated index plus a formal gate report.

Most corpus files carry their date in the filename (``2014-05-06__31.md``), but
17 session-range files (``2016-2017__03.md``) have none, and banner-recovered
dates can be wrong. Dates are therefore cross-checked from three independent
signals -- the manifest date, the sitting-number sequence, and the Marsad
plenary calendar (optional: gates degrade to ``na`` when
``marsad_plenary_dates.txt`` is absent) -- then put through six validation gates.

Pipeline (each stage reads/writes CSVs under ``<root>/out/``):

    triangulate  manifest.csv                -> tn_session_dates_triangulated.csv
    resolve      + document headers          -> tn_session_dates_resolved.csv
    merge        the three above             -> tn_session_dates_final.csv
    gates        final + document bodies     -> tn_session_dates_gated.csv
                                              +  tn_date_gate_report.md

Run one stage (``triangulate`` | ``resolve`` | ``merge`` | ``gates``) or ``all``.
Overridable via environment variables (same convention as the companion script):
    TUNISIA_ROOT         repo root (default: parent of this script's directory)
    TUNISIA_CORPUS_DIR   corpus directory (default: <root>/TN_democratic_cleaned_clean_md)
"""
from __future__ import annotations

import argparse
import collections
import csv
import os
import re
from dataclasses import dataclass
from datetime import date
from pathlib import Path

# --------------------------------------------------------------------------- paths
DEFAULT_ROOT = Path(os.environ.get("TUNISIA_ROOT", Path(__file__).resolve().parents[1]))
SCRIPT_DIR = Path(__file__).resolve().parent


@dataclass(frozen=True)
class Paths:
    """Filesystem layout for one pipeline run (overridable for tests)."""

    out: Path
    manifest: Path
    marsad: Path
    search_dirs: tuple[Path, ...]

    @classmethod
    def default(cls, root: Path = DEFAULT_ROOT) -> "Paths":
        corpus = Path(os.environ.get("TUNISIA_CORPUS_DIR",
                                     str(root / "TN_democratic_cleaned_clean_md")))
        return cls(out=root / "out", manifest=SCRIPT_DIR / "manifest.csv",
                   marsad=SCRIPT_DIR / "marsad_plenary_dates.txt",
                   search_dirs=(corpus,))

    def __getattr__(self, name: str) -> Path:  # triangulated/resolved/final/gated/report
        try:
            return self.out / _CSV_NAMES[name]
        except KeyError:
            raise AttributeError(name)


_CSV_NAMES = {
    "triangulated": "tn_session_dates_triangulated.csv",
    "resolved": "tn_session_dates_resolved.csv",
    "final": "tn_session_dates_final.csv",
    "gated": "tn_session_dates_gated.csv",
    "report": "tn_date_gate_report.md",
}

# ------------------------------------------------------------------ tunable limits
MARSAD_LO, MARSAD_HI = date(2014, 12, 1), date(2021, 7, 31)  # Marsad calendar coverage
HIJRI_TOL = 2       # tabular vs observed Hijri differ by up to ~2 days
COLLISION_MAX = 3   # the ARP can sit 3x/day; >=4 sittings on one date = OCR failure
HEADER_LINES = 120  # masthead search depth

# ----------------------------------------------------------------- Arabic calendars
ARABIC_DIGITS = str.maketrans("٠١٢٣٤٥٦٧٨٩", "0123456789")
_TASHKEEL = re.compile("[" + "".join(chr(c) for c in (
    *range(0x0610, 0x061B), *range(0x064B, 0x0660), 0x0670, *range(0x06D6, 0x06EE))) + "]")


def norm(s: str) -> str:
    """Fold Arabic text for matching: strip tashkeel, Latinise digits, unify hamza/alef,
    ya/alef-maqsura and ta-marbuta. Month and weekday keys below are in this folded form."""
    s = _TASHKEEL.sub("", s).translate(ARABIC_DIGITS)
    return (s.replace("أ", "ا").replace("إ", "ا").replace("آ", "ا")
             .replace("ى", "ي").replace("ة", "ه"))


GREG_MONTHS = {  # Tunisian + Maghrebi + Mashriqi month names -> month number
    "جانفي": 1, "يناير": 1, "فيفري": 2, "فبراير": 2, "مارس": 3,
    "افريل": 4, "ابريل": 4, "ماي": 5, "مايو": 5, "جوان": 6, "يونيو": 6, "يونيه": 6,
    "جويليه": 7, "يوليو": 7, "يوليوز": 7, "اوت": 8, "اغسطس": 8,
    "سبتمبر": 9, "شتنبر": 9, "اكتوبر": 10, "نوفمبر": 11, "نونبر": 11, "ديسمبر": 12, "دجنبر": 12,
}
WEEKDAYS = {  # -> date.weekday(): Mon=0 .. Sun=6
    "الاحد": 6, "الاثنين": 0, "الثلاثاء": 1, "الثلاثا": 1,
    "الاربعاء": 2, "الاربعا": 2, "الخميس": 3, "الجمعه": 4, "السبت": 5,
}
HIJRI_MONTHS = {
    "محرم": 1, "صفر": 2, "ربيع الاول": 3, "ربيع اول": 3,
    "ربيع الثاني": 4, "ربيع الاخر": 4, "ربيع ثاني": 4,
    "جمادي الاولي": 5, "جمادي الاول": 5, "جمادي الثانيه": 6, "جمادي الاخره": 6, "جمادي الثاني": 6,
    "رجب": 7, "شعبان": 8, "رمضان": 9, "شوال": 10,
    "ذو القعده": 11, "ذي القعده": 11, "ذو الحجه": 12, "ذي الحجه": 12,
}


def _alt(keys) -> str:
    return "|".join(sorted(keys, key=len, reverse=True))  # longest-first so multiword wins


RE_GREG_AR = re.compile(r"(\d{1,2})\s+(" + _alt(GREG_MONTHS) + r")\s+(\d{4})")
RE_GREG_NUM = re.compile(r"(\d{1,2})[-/.](\d{1,2})[-/.](\d{4})")
RE_WEEKDAY = re.compile(r"يوم\s+(" + _alt(WEEKDAYS) + r")")
RE_HIJRI = re.compile(r"(\d{1,2})\s+(" + _alt(HIJRI_MONTHS) + r")\s+(1[34]\d{2})")
# a weekday token only counts if a real date phrase follows it
RE_DATE_SIGNAL = re.compile(r"\d{1,2}\s+(?:" + _alt(HIJRI_MONTHS) + "|" + _alt(GREG_MONTHS)
                            + r")|1[34]\d{2}|(?:19|20)\d{2}")

# MENA_PERC doc_id stems: date-named files (2014-05-06__31, 2011-11-19__seg040,
# 2014-01-26__14_Revision_...) and undated session-range files (2016-2017__03).
STEM_RANGE = re.compile(r"^(?P<y1>\d{4})-(?P<y2>\d{4})__(?P<seq>\d+)$")
STEM_DATE = re.compile(r"^(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})__(?P<seg>seg)?(?P<seq>\d+)?")
RE_YEAR_TOK = re.compile(r"(19\d{2}|20\d{2})")


def stem_period_seq(stem: str) -> tuple[str, int | None, str]:
    """(period, seq, series) for a doc_id stem.

    period is the parliamentary year ``YYYY-YYYY`` (Sep..Aug) -- taken verbatim
    from range stems, derived from the filename date otherwise.  seq is the
    trailing sitting/segment number (None when the suffix is prose).  series
    separates numbering sequences that must never share a backbone batch:
    ``seg``  NCA segNNN splits (their own numbering);
    ``ext``  Aug-Sep extraordinary sittings, numbered 01..NN independently of
             the ordinary session (e.g. 2015-08-03__01 .. 2015-10-01__10);
    ``num``  ordinary-session sitting numbers."""
    m = STEM_RANGE.match(stem)
    if m:
        return f"{m['y1']}-{m['y2']}", int(m["seq"]), "num"
    m = STEM_DATE.match(stem)
    if not m:
        return "", None, ""
    y, mo = int(m["y"]), int(m["m"])
    period = f"{y}-{y + 1}" if mo >= 9 else f"{y - 1}-{y}"
    seq = int(m["seq"]) if m["seq"] else None
    series = "seg" if m["seg"] else ("ext" if mo in (8, 9) else "num")
    return period, seq, series


def stem_date(stem: str) -> date | None:
    """The date embedded in a date-named stem, if any."""
    m = STEM_DATE.match(stem)
    return make_date(m["y"], m["m"], m["d"]) if m else None

# --------------------------------------------------------------------- date helpers


def parse_iso(s: str) -> date | None:
    try:
        y, m, d = map(int, s.split("-"))
        return date(y, m, d)
    except (ValueError, AttributeError):
        return None


def make_date(y, m, d) -> date | None:
    try:
        return date(int(y), int(m), int(d))
    except (ValueError, TypeError):
        return None


def hijri_to_greg(y: int, m: int, d: int) -> date | None:
    """Tabular (Kuwaiti-algorithm) Hijri -> Gregorian; within ~2 days of the observed date."""
    jd = (int((11 * y + 3) / 30) + 354 * y + 30 * m - int((m - 1) / 2) + d + 1948440 - 385)
    l = jd + 68569
    n = (4 * l) // 146097
    l -= (146097 * n + 3) // 4
    i = (4000 * (l + 1)) // 1461001
    l = l - (1461 * i) // 4 + 31
    j = (80 * l) // 2447
    gd = l - (2447 * j) // 80
    l = j // 11
    gm = j + 2 - 12 * l
    gy = 100 * (n - 49) + i + l
    return make_date(gy, gm, gd)


def ref_band(*tokens: str) -> tuple[int, int] | None:
    """Plausible calendar-year band from filename/stem tokens, independent of the date
    under test. Old OCR spews stray 4-digit runs; dates outside this band are treated as
    noise (na), not failures."""
    ys = [int(y) for y in RE_YEAR_TOK.findall(" ".join(tokens))]
    return (min(ys) - 1, max(ys) + 1) if ys else None


def in_band(d: date, band: tuple[int, int] | None) -> bool:
    return band is None or band[0] <= d.year <= band[1]


def lis_backbone(items: list[tuple]) -> set:
    """items: [(key, date)]. Return the keys on the longest non-decreasing run of dates
    (ties keep the earliest) -- the self-consistent sequence backbone."""
    n = len(items)
    if not n:
        return set()
    best, prev = [1] * n, [-1] * n
    for i in range(n):
        for j in range(i):
            if items[j][1] <= items[i][1] and best[j] + 1 > best[i]:
                best[i], prev[i] = best[j] + 1, j
    k = max(range(n), key=lambda i: best[i])
    out = set()
    while k != -1:
        out.add(items[k][0])
        k = prev[k]
    return out


def period_bounds(period: str) -> tuple[date, date] | tuple[None, None]:
    """Parliamentary-year span for a ``YYYY-YYYY`` batch: Sep 1 .. Aug 31."""
    if not period:
        return None, None
    return date(int(period[:4]), 9, 1), date(int(period[5:9]), 8, 31)


def load_marsad(path: Path) -> set:
    """Marsad plenary calendar; optional -- an absent file just disables the gate."""
    if not path.exists():
        return set()
    return {d for d in (parse_iso(l.strip()) for l in path.open()) if d}


def load_tiers(p: Paths) -> list[dict]:
    """Normalise manifest.csv rows to the pipeline's tier schema."""
    return [{"session_stem": r["doc_id"], "sitting_date": r["sitting_date"],
             "date_source": r["date_source"], "source_file": Path(r["md_file"]).name}
            for r in read_rows(p.manifest)]


def find_source(name: str, search_dirs) -> Path | None:
    for d in search_dirs:
        p = Path(d) / name
        if p.exists():
            return p
    return None


def scan_dates(lines, band=None) -> list[date]:
    """All Gregorian dates (Arabic-month or numeric) in ``lines``, optionally band-filtered."""
    out = []
    for line in lines:
        t = norm(line)
        for dd, mon, yy in RE_GREG_AR.findall(t):
            d = make_date(yy, GREG_MONTHS[mon], dd)
            if d and in_band(d, band):
                out.append(d)
        for dd, mm, yy in RE_GREG_NUM.findall(t):
            d = make_date(yy, mm, dd)
            if d and in_band(d, band):
                out.append(d)
    return out


def read_rows(path: Path) -> list[dict]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_rows(path: Path, cols, rows) -> None:
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        w.writerows(rows)


# ============================================================== stage 1: triangulate
def triangulate(p: Paths) -> None:
    """Cross-check each date against the sitting-number sequence, the parliamentary-year
    bounds and the Marsad calendar. Flag conflicts; suggest a year-swap repair only when it
    lands on a Marsad plenary day inside the sequence window. Nothing is silently rewritten."""
    marsad = load_marsad(p.marsad)
    rows = load_tiers(p)

    batches = collections.defaultdict(list)
    for i, r in enumerate(rows):
        period, seq, series = stem_period_seq(r["session_stem"])
        r["_period"], r["_seq"] = period, seq
        if period and seq is not None:
            batches[(period, series)].append(i)

    backbone, window = set(), {}
    for idxs in batches.values():
        idxs.sort(key=lambda i: rows[i]["_seq"])
        dated = [(i, parse_iso(rows[i]["sitting_date"])) for i in idxs]
        dated = [(i, d) for i, d in dated if d]
        bb = lis_backbone(dated)
        backbone |= bb
        anchors = [(i, d) for i, d in dated if i in bb]
        for i in idxs:
            seq = rows[i]["_seq"]
            lo = max((d for j, d in anchors if rows[j]["_seq"] < seq), default=None)
            hi = min((d for j, d in anchors if rows[j]["_seq"] > seq), default=None)
            window[i] = (lo, hi)

    n_hard = n_soft = 0
    out = []
    for i, r in enumerate(rows):
        raw = r["sitting_date"]
        d = parse_iso(raw)
        period, seq = r["_period"], r["_seq"]
        lo, hi = window.get(i, (None, None))
        b_lo, b_hi = period_bounds(period)

        in_marsad = seq_ok = in_bounds = ""
        flag, final, suggested = "ok", raw, ""

        if d is None:
            flag, final = "conflict:no_date", ""
        else:
            if marsad and MARSAD_LO <= d <= MARSAD_HI:
                in_marsad = "yes" if d in marsad else "no"
            if b_lo:
                in_bounds = "yes" if b_lo <= d <= b_hi else "no"
            if seq is not None:
                seq_ok = "yes" if i in backbone else "no"

            # Hard evidence is a sequence break or an out-of-period date; a Marsad miss
            # alone is only soft (the calendar is 65-91% complete).
            problems = []
            if seq_ok == "no":
                problems.append("seq_break")
            if in_bounds == "no":
                problems.append("out_of_period")

            # A date carried by the filename itself outranks the sitting-number
            # sequence (numbering series in this corpus restart mid-year): keep
            # the date, record the anomaly as soft for the gates to arbitrate.
            if problems and d == stem_date(r["session_stem"]):
                flag = "soft:" + "+".join(problems)
                problems = []
                n_soft += 1
            elif not problems and in_marsad == "no":
                flag = "soft:not_marsad_plenary"
                n_soft += 1
            if problems:
                final = ""
                for yy in ({int(period[:4]), int(period[5:9])} if period else ()):
                    cand = make_date(yy, d.month, d.day)
                    if cand and cand != d and cand in marsad \
                            and (lo is None or lo <= cand) and (hi is None or cand <= hi):
                        suggested = cand.isoformat()
                        break
                flag = "repaired_year" if suggested else "conflict:" + "+".join(problems)
                final = suggested
                n_hard += 1
            elif in_marsad == "" and seq_ok == "":
                flag = "ok_unverifiable"

        out.append({
            "session_stem": r["session_stem"], "date_raw": raw, "date_source": r["date_source"],
            "period": period, "seq": "" if seq is None else seq,
            "in_marsad": in_marsad, "seq_ok": seq_ok, "in_bounds": in_bounds,
            "date_flag": flag, "final_date": final, "suggested_date": suggested,
            "window_lo": lo.isoformat() if lo else "", "window_hi": hi.isoformat() if hi else "",
        })

    write_rows(p.triangulated, list(out[0]), out)
    print(f"triangulate: {len(rows)} stems, {n_hard} hard-flagged, {n_soft} soft (Marsad miss only)")


# ============================================================= stage 2: resolve
def resolve(p: Paths) -> None:
    """For each hard conflict, try three deterministic fixes in order: a date in the document
    header inside the sequence window (text_window); neighbours pinning it exactly (squeeze);
    a unique Marsad plenary day in the window (marsad_unique). Survivors stay flagged."""
    marsad = load_marsad(p.marsad)
    src_of = {r["session_stem"]: r["source_file"] for r in load_tiers(p)}

    n_res = n_conf = 0
    out = []
    for r in read_rows(p.triangulated):
        if not r["date_flag"].startswith("conflict"):
            continue
        n_conf += 1
        stem = r["session_stem"]
        lo, hi = parse_iso(r["window_lo"]), parse_iso(r["window_hi"])
        b_lo, b_hi = period_bounds(r["period"])
        w_lo = max((d for d in (lo, b_lo) if d), default=None)
        w_hi = min((d for d in (hi, b_hi) if d), default=None)

        resolved_date, how, cands = "", "", []

        path = find_source(src_of.get(stem, ""), p.search_dirs)
        if path and w_lo and w_hi:
            with path.open(encoding="utf-8", errors="replace") as f:
                head = [next(f, "") for _ in range(HEADER_LINES)]
            found = [d for d in scan_dates(head) if w_lo <= d <= w_hi]
            if found:
                top, k = collections.Counter(found).most_common(1)[0]
                resolved_date, how = top.isoformat(), f"text_window(n={k})"
                cands = sorted({d.isoformat() for d in found})

        if not resolved_date and lo and hi and lo == hi:
            resolved_date, how = lo.isoformat(), "squeeze"

        if not resolved_date and w_lo and w_hi:
            inside = sorted(d for d in marsad if w_lo <= d <= w_hi and d not in (lo, hi))
            cands = [d.isoformat() for d in inside]
            if len(inside) == 1:
                resolved_date, how = inside[0].isoformat(), "marsad_unique"

        if resolved_date:
            n_res += 1
        out.append({
            "session_stem": stem, "date_raw": r["date_raw"], "date_flag": r["date_flag"],
            "window_lo": r["window_lo"], "window_hi": r["window_hi"],
            "resolved_date": resolved_date, "resolution": how,
            "in_marsad": ("yes" if parse_iso(resolved_date) in marsad else "no")
                         if resolved_date and marsad else "",
            "candidates": ";".join(cands),
        })

    cols = ["session_stem", "date_raw", "date_flag", "window_lo", "window_hi",
            "resolved_date", "resolution", "in_marsad", "candidates"]
    write_rows(p.resolved, cols, out)
    print(f"resolve: {n_conf} conflicts, {n_res} resolved")


# =============================================================== stage 3: merge
def merge(p: Paths) -> None:
    """Fold triangulation + resolution back onto the tier dates -> the final index.

    date_flag: '' clean | verified_marsad | soft:* (kept date, anomaly noted)
               | repaired (any corrected date, spot-check advised)
               | conflict:unresolved (final_date empty, needs manual review)"""
    tiers = {r["session_stem"]: r for r in load_tiers(p)}
    tri = {r["session_stem"]: r for r in read_rows(p.triangulated)}
    res = {r["session_stem"]: r for r in read_rows(p.resolved)}

    n = collections.Counter()
    out = []
    for stem, t in tiers.items():
        flag = tri.get(stem, {}).get("date_flag", "")
        if flag.startswith("conflict") and res.get(stem, {}).get("resolved_date"):
            r = res[stem]
            final_date, source, out_flag, kind = \
                r["resolved_date"], r["resolution"].split("(")[0], "repaired", "repaired"
        elif flag == "repaired_year":
            final_date, source, out_flag, kind = tri[stem]["final_date"], "repaired_year", "repaired", "repaired"
        elif flag.startswith("conflict"):
            final_date, source, out_flag, kind = "", t["date_source"], "conflict:unresolved", "unresolved"
        else:
            final_date, source = t["sitting_date"], t["date_source"]
            out_flag = {"ok": "verified_marsad", "soft:not_marsad_plenary": flag}.get(flag, "")
            kind = "clean"
        n[kind] += 1
        out.append({"session_stem": stem, "source_file": t["source_file"],
                    "final_date": final_date, "date_source": source, "date_flag": out_flag})

    write_rows(p.final, ["session_stem", "source_file", "final_date", "date_source", "date_flag"], out)
    print(f"merge: {sum(n.values())} stems  {dict(n)}")


# =============================================================== stage 4: gates
GATE_NAMES = ("weekday", "hijri", "recurrence", "marsad", "sequence", "collision")
HARD_GATES = ("recurrence", "sequence", "collision")  # structural
SOFT_GATES = ("weekday", "hijri", "marsad")            # fallible OCR / partial-calendar witnesses


def doc_gates(path: Path | None, fd: date, band) -> tuple[str, str, str, dict]:
    """In-document gates -> (weekday, hijri, recurrence) each pass/fail/na, plus evidence.
    ``band`` guards every in-document date against stray OCR year-runs."""
    wk = hj = rc = "na"
    ev: dict = {}
    if not path:
        return wk, hj, rc, ev
    with path.open(encoding="utf-8", errors="replace") as f:
        lines = f.readlines()
    head = norm(" ".join(lines[:HEADER_LINES]))

    m = RE_WEEKDAY.search(head)
    if m and RE_DATE_SIGNAL.search(head[m.end():m.end() + 40]):
        wk = "pass" if fd.weekday() == WEEKDAYS[m.group(1)] else "fail"
        ev["weekday"] = m.group(1)

    m = RE_HIJRI.search(head)
    if m:
        g = hijri_to_greg(int(m.group(3)), HIJRI_MONTHS[m.group(2)], int(m.group(1)))
        if g and in_band(g, band):
            hj = "pass" if abs((g - fd).days) <= HIJRI_TOL else "fail"
            ev["hijri"] = f"{m.group(1)} {m.group(2)} {m.group(3)}->{g.isoformat()}"

    dates = scan_dates(lines, band)
    if dates:
        cnt = collections.Counter(dates)
        top_date, top_n = cnt.most_common(1)[0]
        if top_n >= 2:
            if top_date == fd or cnt.get(fd, 0) >= max(2, top_n - 1):
                rc = "pass"
            elif top_n >= 3 and cnt.get(fd, 0) == 0:
                rc = "fail"
                ev["_suggest"] = top_date.isoformat()  # body-attested alternative
            ev["recurrence"] = f"mode={top_date.isoformat()}x{top_n}"
    return wk, hj, rc, ev


def verdict_of(fd, gates: dict) -> str:
    if not fd:
        return "no_date"
    hard_pass = sum(gates[g] == "pass" for g in HARD_GATES)
    hard_fail = sum(gates[g] == "fail" for g in HARD_GATES)
    soft_pass = sum(gates[g] == "pass" for g in SOFT_GATES)
    soft_fail = sum(gates[g] == "fail" for g in SOFT_GATES)
    corroborated = hard_pass + soft_pass
    if hard_fail:
        return "conflict"                    # structural disagreement
    if soft_fail:
        return "soft_flag" if corroborated else "conflict"
    if corroborated >= 2:
        return "confirmed"
    return "single_witness" if corroborated == 1 else "unverifiable"


def gates(p: Paths) -> None:
    marsad = load_marsad(p.marsad)
    rows = read_rows(p.final)
    fd_of = {r["session_stem"]: parse_iso(r["final_date"]) for r in rows}

    # sequence backbone per batch, from the final dates
    batches = collections.defaultdict(list)
    for r in rows:
        period, seq, series = stem_period_seq(r["session_stem"])
        if period and seq is not None:
            batches[(period, series)].append((seq, r["session_stem"]))
    on_backbone = {}
    for items in batches.values():
        items.sort()
        dated = [(stem, fd_of[stem]) for _, stem in items if fd_of[stem]]
        if len(dated) < 3:
            continue
        bb = lis_backbone(list(enumerate(d for _, d in dated)))
        for i, (stem, _) in enumerate(dated):
            on_backbone[stem] = i in bb

    collisions = collections.Counter(fd for fd in fd_of.values() if fd)

    out = []
    for r in rows:
        stem, fd = r["session_stem"], fd_of[r["session_stem"]]
        wk = hj = rc = "na"
        ev: dict = {}
        if fd:
            path = find_source(r["source_file"], p.search_dirs)
            wk, hj, rc, ev = doc_gates(path, fd, ref_band(stem, r["source_file"]))

        ms = ("pass" if fd in marsad else "fail") \
            if marsad and fd and MARSAD_LO <= fd <= MARSAD_HI else "na"
        sq = ("pass" if on_backbone[stem] else "fail") if stem in on_backbone else "na"
        cl = ("fail" if collisions[fd] > COLLISION_MAX else "pass") if fd else "na"

        g = {"weekday": wk, "hijri": hj, "recurrence": rc, "marsad": ms, "sequence": sq, "collision": cl}
        out.append({
            "session_stem": stem, "source_file": r["source_file"],
            "final_date": r["final_date"], "date_source": r["date_source"], **g,
            "n_pass": sum(v == "pass" for v in g.values()),
            "n_fail": sum(v == "fail" for v in g.values()),
            "verdict": verdict_of(fd, g), "suggested_date": ev.get("_suggest", ""),
            "evidence": "; ".join(f"{k}:{v}" for k, v in ev.items() if k != "_suggest"),
        })

    cols = ["session_stem", "source_file", "final_date", "date_source", *GATE_NAMES,
            "n_pass", "n_fail", "verdict", "suggested_date", "evidence"]
    write_rows(p.gated, cols, out)
    _write_report(p.report, out)

    vc = collections.Counter(r["verdict"] for r in out)
    print(f"gates: {len(out)} rows  {dict(vc)}")


def _write_report(path: Path, rows) -> None:
    order = ["confirmed", "single_witness", "unverifiable", "soft_flag", "conflict", "no_date"]
    vc = collections.Counter(r["verdict"] for r in rows)
    cov = {g: collections.Counter(r[g] for r in rows) for g in GATE_NAMES}

    md = ["# Tunisian sitting-date validation — gate report", "",
          f"Total sittings: **{len(rows)}**", "",
          "## Verdict distribution", "", "| verdict | n |", "|---|---|"]
    md += [f"| {v} | {vc[v]} |" for v in order if vc.get(v)]
    md += ["", "## Per-gate coverage (pass / fail / na)", "",
           "| gate | pass | fail | na |", "|---|---|---|---|"]
    md += [f"| {g} | {cov[g]['pass']} | {cov[g]['fail']} | {cov[g]['na']} |" for g in GATE_NAMES]

    flagged = sorted((r for r in rows if r["verdict"] in ("conflict", "soft_flag")),
                     key=lambda r: (r["verdict"] != "conflict", r["session_stem"]))
    sym = {"pass": "✓", "fail": "✗", "na": "·"}
    md += ["", "Gates: **wk** weekday · **hj** hijri · **rc** recurrence · **ma** marsad · "
           "**sq** sequence · **co** collision (✓ pass, ✗ fail, · n/a). *suggest* = "
           "body-attested date when recurrence rejects the assigned one.", "",
           f"## Flagged sittings ({len(flagged)})", "",
           "| stem | date | src | wk | hj | rc | ma | sq | co | verdict | suggest | evidence |",
           "|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for r in flagged:
        cells = [sym[r[g]] for g in GATE_NAMES]
        md.append("| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |".format(
            r["session_stem"], r["final_date"] or "—", r["date_source"], *cells,
            r["verdict"], r["suggested_date"] or "", r["evidence"] or ""))
    path.write_text("\n".join(md) + "\n", encoding="utf-8")


# ===================================================================== cli
STAGES = {"triangulate": triangulate, "resolve": resolve, "merge": merge, "gates": gates}


def run_all(p: Paths) -> None:
    for fn in (triangulate, resolve, merge, gates):
        fn(p)


def main(argv=None) -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("stage", choices=[*STAGES, "all"], help="pipeline stage to run")
    ap.add_argument("--root", type=Path, default=DEFAULT_ROOT,
                    help="MENA_PERC repo root (corpus dir + out/ live under it)")
    args = ap.parse_args(argv)
    p = Paths.default(args.root)
    p.out.mkdir(parents=True, exist_ok=True)
    (run_all if args.stage == "all" else STAGES[args.stage])(p)


if __name__ == "__main__":
    main()
