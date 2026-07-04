# Tunisian Parliament Minutes — post-2010 production corpus

Unified, clean markdown for every post-2010 sitting of the Tunisian parliament
(Mudawalat, 2009–2025), plus a deterministic speaker-attributed speeches CSV.
Rebuilt 2026-07-04 (v2 layout: uniform doc_ids, deduped sources, page-furniture
and imprint back-matter stripped, mid-sentence line breaks healed, multi-day
sitting dates unified, vote-tally/session-adjournment leakage removed from
speech text).

## Layout

```
md/                      one .md per sitting (548 documents)
manifest.csv             doc_id, source PDF, engine, sitting date, dropped duplicates
speeches_post2010.csv        all 32,491 turns
speeches_post2010_speechonly.csv   28,103 speech turns (block_type speech/speech_like)
build_speeches_csv.py    regenerates both CSVs from md/ (deterministic)
build_production_post2010.py  regenerates md/ + manifest.csv from the raw extractions
handoff_window_assignment.py   the canonical attribution engine (kept in sync with tunisia_minutes/)
3_mp_roster_911.csv      MP roster: legislatures 2009-2011, 2011-2014, 2014-2019, 2019-2024, 2023-2027
```

## Document naming (uniform doc_id)

- `YYYY-MM-DD__<source-tag>` — exact sitting date (the vast majority of docs,
  including all 50 NCA sittings — see below)
- `YYYY-YYYY__<source-tag>` — parliamentary-year known, exact date not (17
  docs; source PDF's year-of-month digits are corrupted at the source and no
  reliable date could be recovered — see below)

Every file starts with provenance comments (`doc_id`, `source_pdf`, `engine`,
`sitting_date`). Dates come filename-first; in-document banner dates are used
only when plausible for the session (some source fonts corrupt digits).

## What changed in this rebuild

- **Deduping**: the raw corpus stores the same sitting PDF under up to 4
  names/folders (e.g. `todo/2009-2010/2010-01-12-11.pdf` ==
  `todo/2010/2010-01-12-N_11.pdf`, byte-identical). 159 duplicate PDFs were
  collapsed by content hash; `manifest.csv` lists the dropped alternate paths
  per doc in `duplicate_pdfs_dropped`.
- **doc_id scheme unified**: the old `YYYY-MM-DD__<session-range>__<stem>`
  triplet naming (e.g. `2010-01-12__2009-2010__2010-01-12-N_11`, which read as
  spanning two years) is replaced by a single `date__tag` or `range__tag` id.
- **Page furniture removed from both engines**: running headers
  (`مداولات مجلس نواب الشعب - جلسة...`, tatweel-stretched
  `مـداولات مجلس النـواب`, and their column/line-wrapped truncations) and
  per-page issue banners (`عدد 12 الأربعاء 04 فيفري 2015`) no longer leak into
  speech text — previously ~2,700+ turns carried a stray header line.
- **Imprint back-matter truncated**: the last printed page of each bulletin
  (subscription price, phone number, "حساب دعم النشاط الفكري" account note) is
  detected and dropped from the tail instead of surviving as trailing blocks.
- **Mid-sentence paragraph splits healed**: text hard-wrapped across a
  page/column break (block ends in an Arabic letter, no terminal punctuation,
  next block is plain prose) is rejoined. Measured before/after: born-digital
  block boundaries that split mid-sentence dropped from ~32% to ~4% (residual
  is mostly genuine short-block boundaries the heuristic correctly leaves alone).
- **Speech-window boundary now recognizes session-adjournment formulas**
  ("رفعت الجلسة على الساعة...", "كانت الساعة...") so a speech turn stops there
  instead of absorbing whatever text (often another sitting's furniture)
  follows in the source file.
- **Legislature/period assignment now date-based**, not filename-prefix
  guessing: exact sitting dates are bucketed against the five known terms
  (Chamber of Deputies 2009-2011, NCA 2011-2014, ARP-I 2014-2019, ARP-II
  2019-2024, ARP-III 2023-2027); session-range-only files fall back to the
  parliamentary year. This fixed roster lookups for files near term boundaries.
- **All post-2010 source folders now included**: `todo/2023` (21 PDFs),
  `extraordinary_session_2015` (10), `extraordinary_session_2016` (4) were
  missing from the first build and are now in production.
- **2023-2027 roster added**: 154 members of the current ARP appended to
  `3_mp_roster_911.csv`, sourced from jawharafm.net and journalistesfaxien.tn
  (cross-checked, agreeing lists; arp.tn and Wikipedia were unavailable).
- **Multi-day sitting dates unified**: ~13 source PDFs are filed under BOTH
  calendar days a sitting spans (e.g. `2024-07-_17_-_16_N_-_47.pdf`), which
  previously produced garbled doc_ids embedding both days plus the session
  number (`2023-2024__2024_07_17_16_47`). These are now dated to the banner-
  verified first day and tagged by session number alone (`2024-07-16__N47`).
- **Vote-tally and session-adjournment leakage removed from speech text**: a
  chair reading out a roll-call result ("115 صوتا نعم مع احتفاظ5 أصوات
  ومعارضة10 أصوات") or closing a sitting mid-paragraph ("...ونرفع الجلسة
  (كانت الساعة الرابعة...)") is now a hard stop for the speech-capture window;
  previously the window kept growing past these markers, up to 65,940 stray
  characters of next-section content in one case. Measured: turns with >300
  characters of leaked content after an adjournment mention dropped from 214
  to 6 (all 6 remaining are MPs rhetorically quoting the phrase mid-speech,
  correctly left alone). A law-article citation with no space before the
  digit ("الفصل6" vs "الفصل 6") is now also recognized as a boundary — this
  form alone accounted for 7,657 previously-unmatched blocks.
- **Session-range-only dates recovered from the in-body banner**: 108 docs
  were previously dated only to a parliamentary year (`YYYY-YYYY__<tag>`)
  because their source PDF filename carries no date at all (named just by
  session number, e.g. `72.pdf`) — but the exact date is printed in the
  document's own opening banner ("... جلسة عامة يوم الاثنين 24 جويلية 2017").
  77 of these now carry a recovered exact date; the remaining 17 have
  corrupted year digits at the source (e.g. "٢٠١٦" rendered as "2116") and
  were deliberately left as year-range-only rather than guessed.
- **NCA tomes split into individual dated sittings**: the 39 "Tome_N_partNN"
  files each bundled several sittings (boundary-marked internally by "عقد
  المجلس الوطني التأسيسي جلسة عامة", 1-5 per part, tracked as one continuous
  stream since some parts are pure same-day continuations with zero boundary
  markers of their own). Now split into 50 individual `YYYY-MM-DD__segNNN`
  documents, each dated from its own opening banner or carried forward from
  the previous sitting when a same-day evening continuation has no date of
  its own. A true session-number extraction was attempted and dropped: each
  segment opens with a table-of-contents-style preamble listing *upcoming*
  sittings, which polluted a naive "session number" regex with numbers
  belonging to a different sitting — the `segNNN` tag is an honest sequential
  placeholder, not a recovered official session number.

## Extraction engines (see `engine` in manifest.csv)

- `borndigital_deterministic_v2` (395 docs): text layer read directly from the
  PDF with exact glyph geometry — perfect right-to-left column order, flipped
  lam-ligatures repaired, reversed digit runs repaired where provable. No OCR.
- `mistral_ocr4_colsplit` (153 docs): scanned PDFs (and born-digital files
  whose embedded fonts corrupt letters beyond repair) rendered, split at the
  column gutter, and OCR'd column-by-column — column order guaranteed by
  construction.

## Speaker attribution quality (speech turns)

| legislature | turns | MP identity resolved | role known | raw name only |
|---|---|---|---|---|
| 2009-2011 | 1,290 | 33.6% | 34.5% | 31.9% |
| 2011-2014 | 1,786 | 46.7% | 12.8% | 40.5% |
| 2014-2019 | 16,074 | 51.4% | 17.2% | 31.5% |
| 2019-2024 | 2,404 | 55.2% | 15.8% | 29.0% |
| 2023-2027 | 6,549 | 49.8% | 20.9% | 29.3% |
| **Overall** | **28,103** | **50.2%** | **18.4%** | **31.3%** |

Every turn is attributed by a hand-off cue name/role even when it doesn't
resolve to a roster mp_id — 100% of turns carry at least a raw speaker string
or role. 69.1% resolve to either a roster MP identity or a formal role (chair,
minister, rapporteur).

## Known residual gaps

- A handful of turns (~5 of 28,197) still contain fragments of imprint text on
  pages where OCR corrupted the header/imprint markers themselves beyond
  regex recognition — negligible, not worth a bespoke rule.
- By-election / replacement members for 2023-2027 are not yet in the roster
  (only the 154 initially elected).

## Rebuild

```
python3 build_production_post2010.py   # regenerate md/ + manifest.csv (needs raw corpus)
python3 build_speeches_csv.py          # regenerate both CSVs from md/
```

No network, no OCR, no LLM in either step — byte-reproducible from the raw
extraction sources (or from `md/` alone for the CSV step).
