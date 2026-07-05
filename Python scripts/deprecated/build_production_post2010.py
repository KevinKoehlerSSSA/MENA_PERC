#!/usr/bin/env python3
"""Build the unified post-2010 production corpus (v2 layout).

Sources, with precedence:
  1. _determ_results_v2/  — born-digital deterministic extraction (v2 fixes),
     EXCEPT the mode-A letter-corrupt files listed in
     _colsplit_results/_extra_queue.txt, which are replaced by their OCR.
  2. _colsplit_results/<post-2010 folders>/ — column-split Mistral OCR.

What this version fixes over the first production build:
  * ONE uniform doc_id scheme:
        YYYY-MM-DD__<n>     exact sitting date + cleaned source tag
        YYYY-YYYY__<n>      parliamentary-year range only
        2011-2014__Tome_*   NCA tomes (whole 2011-2014 term)
    (no more date__session-range__date-stem triplets)
  * duplicate sources collapsed: the same sitting PDF exists under up to four
    names/folders (todo/2009-2010 + todo/2010, "-11" + "-N_11") — deduped by
    PDF md5 and again by extracted-body hash
  * page furniture stripped from BOTH engines (running headers incl. tatweel
    variants "مـداولات مجلس النـواب", clipped "…- جلسة يوم", per-page issue
    banners "عدد 12 الأربعاء 04 فيفري 2015", page numbers)
  * trailing subscription/imprint back-matter truncated (الرائد الرسمي, ثمن
    العدد, الاشتراك, الهاتف, حساب دعم النشاط الفكري …)
  * mid-sentence paragraph splits healed: a block whose last line is long
    (hard-wrapped), ends in an Arabic letter with no terminal punctuation is
    joined with the next block when that block is plain running prose
"""
import os, re, sys, csv, glob, hashlib
from collections import defaultdict

sys.path.insert(0, "/home/gena/mudalawat_pdfs/pipeline")
from tn_borndigital_extract import is_furniture_block

ROOT = "/home/gena/mudalawat"
TODO = os.path.join(ROOT, "todo")
OUT = os.path.join(ROOT, "production_post2010")
MD = os.path.join(OUT, "md")
os.makedirs(MD, exist_ok=True)

# manual date overrides: source stems whose banner/filename dates are corrupt
# (Djerba-attack sitting no. 10 held 11-12 & 16-18 May 2023; banner digits
# reversed to "0202"): date = first day of the multi-day record.
DATE_OVERRIDES = {"2023-05--_16_-18_-11_N_-_10": ("2023-05-11", "10", "manual_multiday_first_day")}

# multi-day sittings where the filename carries both calendar days the sitting
# spans (PDF filed under its LAST day, in-document banner gives the FIRST).
# Verified against banners for every shape below: first day = the smaller of
# the two day numbers. -> (date of first day, "N<session>")
MULTIDAY_A = re.compile(r"^(\d{4})-(\d{2})-_?(\d{1,2})[-_]+(\d{1,2})_N[-_]+(\d+)$")  # 2024-02-7_-_6_N_-_32, 2016-03-30_29_N_44
MULTIDAY_B = re.compile(r"^(\d{1,2})[-_]+(\d{1,2})_(\d{2})-(\d{4})_N[-_]+(\d+)$")     # 07_08_01-2025_N_-_32, 25-26_12-2024_N_-_30

def multiday_override(stem):
    m = MULTIDAY_A.match(stem)
    if m:
        y, mo, d1, d2, sess = m.groups()
        day = min(int(d1), int(d2))
        return (f"{y}-{mo}-{day:02d}", f"N{sess}", "multiday_first_day_min")
    m = MULTIDAY_B.match(stem)
    if m:
        d1, d2, mo, y, sess = m.groups()
        day = min(int(d1), int(d2))
        return (f"{y}-{mo}-{day:02d}", f"N{sess}", "multiday_first_day_min")
    return None

OCR_DIRS = ["2010", "2014-2015", "2015-2016", "2016-2017", "2017-2018",
            "2018-2019", "2019-2020", "2020-2021", "2023", "2023-2024",
            "2024-2025", "extraordinary_session_2015",
            "extraordinary_session_2016", "extraordinary_session_2017"]
# NCA tomes ("_root") are handled separately by split_nca_tomes(): each part
# file bundles several sittings, so they can't go through the generic
# one-file-one-candidate OCR loop below.

# ---- date helpers -------------------------------------------------------------
def date_from_stem(stem):
    """YYYY-MM-DD from a source stem: tries YMD then DMY."""
    m = re.search(r"(20\d{2})[-_.](\d{1,2})[-_.](\d{1,2})", stem)
    if m:
        y, mo, d = int(m.group(1)), int(m.group(2)), int(m.group(3))
        if 1 <= mo <= 12 and 1 <= d <= 31:
            return f"{y:04d}-{mo:02d}-{d:02d}"
    m = re.search(r"(\d{1,2})[-_.](\d{1,2})[-_.](20\d{2})", stem)
    if m:
        d, mo, y = int(m.group(1)), int(m.group(2)), int(m.group(3))
        if 1 <= mo <= 12 and 1 <= d <= 31:
            return f"{y:04d}-{mo:02d}-{d:02d}"
    return None

def clean_rest(stem):
    """Source tag for the doc_id: the stem minus its date and filler tokens."""
    s = re.sub(r"(20\d{2})[-_.](\d{1,2})[-_.](\d{1,2})", " ", stem)
    s = re.sub(r"(\d{1,2})[-_.](\d{1,2})[-_.](20\d{2})", " ", s)
    tokens = [t for t in re.split(r"[^0-9A-Za-z]+", s) if t]
    out = []
    for i, t in enumerate(tokens):
        if t in ("N", "n", "No") and i + 1 < len(tokens) and tokens[i + 1].isdigit():
            continue                     # "N_11" copy marker, not content
        out.append(t)
    return "_".join(out)

# ---- text pipeline ------------------------------------------------------------
MARK = re.compile(r"<!--\s*page\s+\d+\s+col\s+[RLS]\s*-->")
IMG = re.compile(r"!\[[^\]]*\]\([^)]*\)")

IMPRINT = re.compile(
    r"الرائد الرسمي|شراء أعداد|ثمن العدد|الاشتراك|لااشتراك|دينارا|الهاتف|الفاكس|"
    r"وكيلة? (?:التسبقات|المقا ?بيض)|وكالة المقابيض|أموال المشاركة|حساب دعم النشاط|"
    r"ميزانية الدولة الجزء|بميزان ?ية الدولة|المطبعة الرسمية|بمصلحة (?:أرشيف|وكالة)|"
    r"عبر البريد|\(?باردو\)?|السنة الاشتراكية|تدفع مبالغ|يمكن دفع|"
    r"م[ـ]*داولات مجلس ال?نـ?[ـ]*واب|مداولات مجلس نواب|^\.{6,}|^[\d\s.:ـ)(–-]+$")

def truncate_imprint_tail(blocks, maxscan=60, maxgap=2):
    """Drop trailing subscription/imprint back-matter (last printed page)."""
    cut = len(blocks)
    matched = gap = 0
    i = len(blocks) - 1
    while i >= 0 and (len(blocks) - 1 - i) < maxscan:
        b = blocks[i]
        if IMPRINT.search(b) or len(b) < 4:
            cut = i
            matched += 1
            gap = 0
        else:
            gap += 1
            if gap > maxgap:
                break
        i -= 1
    return blocks[:cut] if matched >= 2 else blocks

# Recover an exact sitting date from the in-body opening banner ("عقد ... جلسة
# عامة يوم الاثنين 24 جويلية 2017") for files whose SOURCE FILENAME carries no
# date at all (named just by session number, e.g. "72.pdf") — the date exists
# in the document, just not in the filename. Strict form requires the "يوم
# <weekday>" lead-in; loose form (day-month-year alone, valid day required)
# catches banners the strict form misses. First match wins (opening banner).
_MO = {"جانفي": 1, "فيفري": 2, "مارس": 3, "أفريل": 4, "ماي": 5, "جوان": 6,
       "جويلية": 7, "أوت": 8, "سبتمبر": 9, "أكتوبر": 10, "نوفمبر": 11, "ديسمبر": 12}
_MO_ALT = "|".join(_MO)
BANNER_DATE_STRICT = re.compile(rf"يوم\s+\S+\s+(\d{{1,2}})\s+({_MO_ALT})\s+(20\d\d)")
BANNER_DATE_LOOSE = re.compile(rf"(\d{{1,2}})\s+({_MO_ALT})\s+(20\d\d)")

def recover_date_from_body(body):
    m = BANNER_DATE_STRICT.search(body) or BANNER_DATE_LOOSE.search(body)
    if not m:
        return None
    d, mo, y = int(m.group(1)), _MO[m.group(2)], int(m.group(3))
    if not (1 <= d <= 31):
        return None
    return f"{y:04d}-{mo:02d}-{d:02d}"

TERM_END = re.compile(r'[.!؟:؛»"\)\]…]\s*$')
AR_END = re.compile(r"[ء-ي]\s*$")
AR_START = re.compile(r"^[ء-ي]")
NO_MERGE_START = re.compile(
    r"^(?:#{1,6}\s|[-*•]|\d|السيد(?:ة)?\s|النائب(?:ة)?\s|الرئيس\s*:|"
    r"(?:الباب|القسم|الفرع|الفصل)\s|و?رفعت\s+الجلسة|و?كانت\s+الساعة)")

def heal_paragraphs(blocks):
    """Join blocks split mid-sentence by page/column breaks.

    Merge test: previous block's last line is long (hard-wrapped at the column
    measure), ends in an Arabic letter with no terminal punctuation; next block
    starts with a plain Arabic letter and is not a heading / speaker cue /
    boundary formula.  Measured: ~32% of born-digital block boundaries.
    """
    out = []
    for b in blocks:
        if out:
            last_line = out[-1].split("\n")[-1]
            if (len(last_line) >= 60 and AR_END.search(last_line)
                    and not TERM_END.search(last_line)
                    and AR_START.match(b) and not NO_MERGE_START.match(b)
                    and not ("\n" not in b and len(b) < 40)):
                out[-1] = out[-1] + " " + b
                continue
        out.append(b)
    return out

def clean_blocks(text, is_ocr):
    if is_ocr:
        text = MARK.sub("\n", text)
        text = IMG.sub("", text)
    blocks = [p.strip() for p in text.split("\n\n") if p.strip()]
    blocks = [b for b in blocks if not is_furniture_block(b)]
    blocks = truncate_imprint_tail(blocks)
    return heal_paragraphs(blocks)

def doc_blocks(path, is_ocr):
    return clean_blocks(open(path, encoding="utf-8").read(), is_ocr)

# ---- candidate enumeration ----------------------------------------------------
modeA = set()
xq = os.path.join(ROOT, "_colsplit_results/_extra_queue.txt")
if os.path.exists(xq):
    for line in open(xq):
        line = line.strip()
        if line and not line.startswith("#"):
            modeA.add(os.path.splitext(os.path.basename(line))[0])

def find_pdf(stem, qualifier=None):
    cands = []
    if qualifier:
        cands.append(os.path.join(TODO, qualifier, stem + ".pdf"))
    cands.append(os.path.join(TODO, stem + ".pdf"))
    cands.extend(sorted(glob.glob(os.path.join(TODO, "*", stem + ".pdf"))))
    for c in cands:
        if os.path.exists(c):
            return c
    return None

_md5 = {}
def pdf_md5(path):
    if path not in _md5:
        h = hashlib.md5()
        with open(path, "rb") as f:
            for chunk in iter(lambda: f.read(1 << 20), b""):
                h.update(chunk)
        _md5[path] = h.hexdigest()
    return _md5[path]

# ---- NCA tomes (2011-2014): split each part into individual sittings ---------
# Each "Tome_N_partNN" OCR file bundles several sittings, boundary-marked by
# "عقد المجلس الوطني التأسيسي جلسة عامة" (+ a date on the first sitting of a
# day, absent on same-day evening continuations). 8 of the 39 part files have
# ZERO boundary markers at all (they are pure continuations of whatever
# sitting was still open at the end of the previous part) — so sittings are
# tracked as ONE continuous stream across the whole tome corpus, not per file.
# Per user decision: an undated continuation inherits the previous sitting's
# date (same calendar day resuming), never a guessed date.
NCA_BOUND = re.compile(r"عقد\s+المجلس\s+الوطني\s+التأسيسي\s+جلسة\s+عامة")
# NOTE: a "الجلسة العامة عدد N" scan was tried and DROPPED — each segment opens
# with a table-of-contents-style preamble listing UPCOMING sittings ("Session
# No. 72, Tuesday 28 Feb 2012"), so the first "عدد N" hit in a chunk is often
# borrowed from a future/unrelated sitting, not this segment's own number.
# Sequential tags are honest; a real session number was not reliably
# extractable without much deeper structural parsing.

def split_nca_tomes():
    files = sorted(
        glob.glob(os.path.join(ROOT, "_colsplit_results/_root/*_colsplit.md")),
        key=lambda f: tuple(int(x) for x in re.search(r"Tome_(\d+)_part(\d+)", f).groups()),
    )
    segments = []              # list of dict(date, sessnum, parts=[stem,...], text=[...])
    cur = None                 # current open segment
    cur_date = None
    for f in files:
        stem = os.path.basename(f).removesuffix("_colsplit.md")
        text = open(f, encoding="utf-8").read()
        text = MARK.sub("\n", text)
        text = IMG.sub("", text)
        bounds = list(NCA_BOUND.finditer(text))
        if not bounds:
            if cur is not None:
                cur["text"].append(text)
                cur["parts"].append(stem)
            # else: front matter before the very first sitting anywhere (dropped)
            continue
        spans = [(bounds[i].start(), bounds[i + 1].start() if i + 1 < len(bounds) else len(text))
                 for i in range(len(bounds))]
        # text before the first boundary in THIS file continues the open segment
        pre = text[: spans[0][0]]
        if cur is not None and pre.strip():
            cur["text"].append(pre)
            if stem not in cur["parts"]:
                cur["parts"].append(stem)
        for start, end in spans:
            chunk = text[start:end]
            m = recover_date_from_body(chunk)
            if m:
                cur_date = m
            cur = dict(date=cur_date, parts=[stem], text=[chunk])
            segments.append(cur)
    return segments

# candidate: dict(engine, src_md, stem, date, rng, date_source, pdf); NCA
# candidates additionally carry raw_text (assembled in memory) instead of src_md.
cands = []

for i, seg in enumerate(split_nca_tomes()):
    tag = f"seg{i:03d}"
    primary = seg["parts"][0]
    if seg["date"]:
        date, rng, dsrc = seg["date"], None, "banner_or_carried_forward"
    else:
        date, rng, dsrc = None, "2011-2014", "undated_no_prior_banner"
    cands.append(dict(engine="mistral_ocr4_colsplit", src_md=None,
                      raw_text="".join(seg["text"]), stem=primary,
                      date=date, rng=rng, date_source=dsrc, tag=tag,
                      pdf=find_pdf(primary),
                      alt_sources=";".join(f"{p}.pdf" for p in seg["parts"][1:])))

# 1. born-digital determ v2
for f in sorted(glob.glob(os.path.join(ROOT, "_determ_results_v2/*_cleaned_clean.md"))):
    base = os.path.basename(f).removesuffix("_cleaned_clean.md")
    if base.startswith("UNDATED"):
        continue
    m = re.match(r"^(\d{4}-\d{2}-\d{2})__(?:(.*?)__)?(.*)$", base)
    r = re.match(r"^(\d{4})-(\d{4})_(.*)$", base)
    date = rng = None; qualifier = None
    if m:
        date, qualifier, stem = m.group(1), m.group(2), m.group(3)
        dsrc = "filename_or_banner"
    elif r:
        rng, stem = f"{r.group(1)}-{r.group(2)}", r.group(3)
        found = date_from_stem(stem)
        if found:
            date, dsrc = found, "filename"
            rng = None
        else:
            dsrc = "session_range"
    else:
        continue
    tag = None
    if stem in DATE_OVERRIDES:
        date, tag, dsrc = DATE_OVERRIDES[stem]
    elif (ov := multiday_override(stem)):
        date, tag, dsrc = ov
        rng = None
    year = int(date[:4]) if date else int(rng[:4])
    if year < 2010:
        continue
    if stem in modeA:
        continue                          # replaced by OCR below
    cands.append(dict(engine="borndigital_deterministic_v2", src_md=f,
                      stem=stem, date=date, rng=rng, date_source=dsrc, tag=tag,
                      pdf=find_pdf(stem, qualifier)))

# 2. colsplit OCR
for d in OCR_DIRS:
    for f in sorted(glob.glob(os.path.join(ROOT, "_colsplit_results", d, "*_colsplit.md"))):
        stem = os.path.basename(f).removesuffix("_colsplit.md")
        date = rng = tag = None
        if d == "_root":
            rng, dsrc = "2011-2014", "tome_2011_2014"
        elif stem in DATE_OVERRIDES:
            date, tag, dsrc = DATE_OVERRIDES[stem]
        elif (ov := multiday_override(stem)):
            date, tag, dsrc = ov
        else:
            date = date_from_stem(stem)
            dsrc = "filename" if date else "session_range"
            if not date:
                if re.match(r"^\d{4}-\d{4}$", d):
                    rng = d
                else:
                    print(f"  skip UNDATED ocr: {d}/{stem}")
                    continue
        if date and int(date[:4]) < 2010:
            continue
        cands.append(dict(engine="mistral_ocr4_colsplit", src_md=f,
                          stem=stem, date=date, rng=rng, date_source=dsrc, tag=tag,
                          pdf=find_pdf(stem, d if d != "_root" else None)))

# ---- dedupe by source-PDF md5 ---------------------------------------------------
groups = defaultdict(list)
no_pdf = 0
for c in cands:
    if "raw_text" in c:
        # NCA segments: several sittings intentionally share one source PDF —
        # keyed per segment (tag), never collapsed against each other.
        groups[("nca", c["stem"], c["tag"])].append(c)
    elif c["pdf"]:
        groups[pdf_md5(c["pdf"])].append(c)
    else:
        no_pdf += 1
        groups[("nopdf", c["stem"])].append(c)

def pick_key(c):
    return (0 if c["engine"].startswith("borndigital") else 1,
            1 if re.search(r"(?:^|[-_])N[-_]", c["stem"]) else 0,
            len(c["stem"]), c["stem"])

kept, dropped_dup = [], 0
for g in groups.values():
    g.sort(key=pick_key)
    best = g[0]
    if len(g) > 1:
        best["alt_sources"] = ";".join(os.path.relpath(c["pdf"], TODO)
                                       for c in g[1:] if c["pdf"])
    kept.append(best)
    dropped_dup += len(g) - 1

# ---- doc ids + render -----------------------------------------------------------
def make_doc_id(c):
    if c.get("tag"):
        rest = c["tag"]
    else:
        rest = clean_rest(c["stem"])
    prefix = c["date"] or c["rng"]
    return f"{prefix}__{rest}" if rest else f"{prefix}__doc"

manifest, body_hashes = [], {}
n_written = n_bodydup = 0
by_id = {}
n_banner_recovered = 0
for c in sorted(kept, key=lambda c: (c["date"] or c["rng"] or "", c["stem"])):
    if "raw_text" in c:
        blocks = clean_blocks(c["raw_text"], True)
    else:
        blocks = doc_blocks(c["src_md"], c["engine"] == "mistral_ocr4_colsplit")
    body = "\n\n".join(blocks)
    if len(body) < 200:
        print(f"  skip EMPTY: {c['stem']}")
        continue
    if not c["date"] and c["date_source"] == "session_range":
        found = recover_date_from_body(body)
        if found:
            c["date"], c["date_source"], c["rng"] = found, "banner_recovered", None
            n_banner_recovered += 1
    doc_id = make_doc_id(c)
    while doc_id in by_id:
        doc_id += "_b"
    bh = hashlib.md5(body.encode()).hexdigest()
    if bh in body_hashes:
        n_bodydup += 1
        continue
    body_hashes[bh] = doc_id
    by_id[doc_id] = c
    src = os.path.relpath(c["pdf"], TODO) if c["pdf"] else "unknown"
    head = (f"<!-- doc_id: {doc_id} -->\n"
            f"<!-- source_pdf: todo/{src} -->\n"
            f"<!-- engine: {c['engine']} -->\n"
            f"<!-- sitting_date: {c['date'] or 'unknown'} ({c['date_source']}) -->\n\n")
    with open(os.path.join(MD, doc_id + ".md"), "w", encoding="utf-8") as f:
        f.write(head + body)
    manifest.append({"doc_id": doc_id, "md_file": "md/" + doc_id + ".md",
                     "source_pdf": "todo/" + src, "engine": c["engine"],
                     "sitting_date": c["date"] or "", "date_source": c["date_source"],
                     "session_range": c["rng"] or "", "n_chars": len(body),
                     "duplicate_pdfs_dropped": c.get("alt_sources", "")})
    n_written += 1

with open(os.path.join(OUT, "manifest.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["doc_id", "md_file", "source_pdf", "engine",
                                      "sitting_date", "date_source", "session_range",
                                      "n_chars", "duplicate_pdfs_dropped"])
    w.writeheader()
    for row in sorted(manifest, key=lambda r: r["doc_id"]):
        w.writerow(row)

eng = defaultdict(int)
for r in manifest:
    eng[r["engine"]] += 1
print(f"written: {n_written} docs -> {MD}")
print(f"  by engine: {dict(eng)}")
print(f"  duplicate PDFs collapsed: {dropped_dup}; identical bodies dropped: {n_bodydup}; no-pdf candidates: {no_pdf}")
print(f"  dates recovered from in-body banner (session_range -> exact): {n_banner_recovered}")
