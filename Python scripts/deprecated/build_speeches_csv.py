#!/usr/bin/env python3
"""Build speeches_post2010.csv from the production md/ corpus.

Wraps the canonical handoff_window_assignment.py (deterministic cue-window
speaker attribution, 911-MP roster for 2011-2021) over the unified post-2010
markdown files. No OCR, no LLM — fully reproducible.

Usage:  python3 build_speeches_csv.py
Needs:  handoff_window_assignment.py + 3_mp_roster_911.csv next to this file.
Output: speeches_post2010.csv and speeches_post2010_speechonly.csv (block_type
        in {speech, speech_like}), both also gzipped.

Note: mp_id resolution only exists where a roster is loaded (2011-2021 today).
2023-2027 sittings still get speaker names/roles from the hand-off cues; they
resolve to mp_ids as soon as a roster for that legislature is added with
period label "2023-2027".
"""
import os, sys, csv, gzip, shutil, glob, importlib.util
from pathlib import Path

HERE = Path(__file__).resolve().parent
os.environ.setdefault("TUNISIA_YEAR_MIN", "2010")
os.environ.setdefault("TUNISIA_YEAR_MAX", "2026")
os.environ.setdefault("TUNISIA_MP_ROSTER", str(HERE / "3_mp_roster_911.csv"))

spec = importlib.util.spec_from_file_location("hwa", HERE / "handoff_window_assignment.py")
hwa = importlib.util.module_from_spec(spec)
sys.modules["hwa"] = hwa
spec.loader.exec_module(hwa)

csv.field_size_limit(sys.maxsize)
roster = hwa.read_roster(hwa.ROSTER_PATH)
out = HERE / "speeches_post2010.csv"
n_rows = 0
with open(out, "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=hwa.FIELDS, extrasaction="ignore")
    w.writeheader()
    for md in sorted(glob.glob(str(HERE / "md" / "*.md"))):
        p = Path(md)
        meta = hwa.infer_file_meta(p)
        if not meta.include:
            print(f"  skip ({meta.reason}): {p.name}")
            continue
        rows = hwa.extract_turns(p, meta, roster)
        for row in rows:
            w.writerow(row)
        n_rows += len(rows)
print(f"wrote {n_rows} turns -> {out}")

dst = str(out).replace(".csv", "_speechonly.csv")
with open(out, newline="", encoding="utf-8") as f, open(dst, "w", newline="", encoding="utf-8") as g:
    r = csv.reader(f); h = next(r); i_bt = h.index("block_type")
    w = csv.writer(g); w.writerow(h)
    kept = 0
    for row in r:
        if row[i_bt] in ("speech", "speech_like"):
            w.writerow(row); kept += 1
print(f"speechonly: {kept} turns -> {dst}")
for pth in (str(out), dst):
    with open(pth, "rb") as fi, gzip.open(pth + ".gz", "wb") as fo:
        shutil.copyfileobj(fi, fo, length=1 << 20)
print("gzipped both")
