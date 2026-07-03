# Handoff — Speaker-Assignment Pipeline & CorpusReview (2026-07-02)

This document records every change made in the working session of 2 July 2026: what was changed, why, how it was validated, and what remains open.

## 1. Context

The task was to improve the deterministic (regex-based) detection of floor handoffs between MPs in the Tunisian parliamentary minutes corpus (`TN_democratic_cleaned_clean_md/`, 537 markdown files, 2011–2021). The extractor is `Python scripts/handoff_window_assignment_gena.py`. Reported discovery rate at the start was ~40%.

A key structural fact drives the whole design: the cleaned `.md` files contain **no speaker headers**. Chair speech and MP speech alternate as bare paragraphs; the only deterministic signal for a turn change is the chair's handoff language ("الكلمة للسيد…", "تفضل…", etc.). The extractor therefore detects a cue, then captures the following "speech window" until it hits a boundary (next cue, vote formula, section header).

## 2. Measurement methodology

A coverage audit script (session scratch, reproducible) samples 60 files (seed 7), splits them into paragraphs, and computes two metrics:

- **substantive coverage** — share of all substantive paragraphs assigned to some turn window;
- **opener recall** — share of paragraphs that begin with an unambiguous MP-speech opener ("شكرا سيدي الرئيس", "بسم الله"…) that were captured. These openers are near-certain MP turns, so this is the best proxy for true recall.

For every missed opener, the audit dumps the *preceding* paragraph — which must contain the cue the regex failed to parse — enabling systematic clustering of failure modes rather than anecdote-driven patching.

**Results:** baseline 59.8% coverage / 82.1% opener recall → after changes **80.2% coverage / 95.2% opener recall**.

## 3. Changes to `Python scripts/handoff_window_assignment_gena.py`

### 3.1 Cue inventory: 4 → 13 patterns

The original four patterns (الكلمة…, نقطة نظام…, تفضل + name, name + تفضل) were kept (with fixes) and nine added. Each detected cue now records its pattern id (`p0`–`p12`) in `assignment_method`, so per-pattern precision can be audited in the output CSV.

- **p0** الكلمة family — now also matches the OCR variant `الكلمه`; verb prefixes (نعطي/نمرر/أحيل…) are implicitly covered since the match anchors on الكلمة itself.
- **p1** نقطة نظام + name (unchanged in substance).
- **p2** تفضل/تفضلي (+ يا) (+ honorific) + name — honorific set widened (see 3.2).
- **p3** honorific + name + تفضل — **now tolerates a comma** between name and تفضل ("مصطفى بن أحمد، تفضل" previously failed because the regex demanded whitespace only).
- **p4** *thanks-and-call*: `شكرا، السيدة NAME (duration)?` at end of line. The comma **immediately after شكرا** distinguishes "thank you; NAME, your turn" (next speaker) from "thanks TO name" (previous speaker, no comma). A negative lookahead blocks الرئيس/الوزير so MP speech openings like "شكرا سيدي الرئيس" never fire as cues.
- **p5** verb-first handoffs: يتفضل / ستتفضل / وستفضل / ليتفضل / فليتفضل + (honorific) + name (e.g. "وستفضل السيد الخضر بلهوشات، مقرر اللجنة، لتلاوة التقرير").
- **p6** نستمع (الآن) إلى + honorific + name — the honorific is **mandatory**, otherwise mid-speech "لا نستمع إلى بعضنا" produced junk candidates (tightened after precision spot-check).
- **p7** ننتقل/نمر إلى + honorific + name.
- **p8** *time allocation*: honorific + name + (له/لها/لك) N دقائق ("السيدة صفية الخلفي أربع دقائق") — allocating minutes implies giving the floor. الرئيس is blocked after the honorific (MPs talking *about* the chair's timing).
- **p9** oral questions: السؤال … من قبل/موجه من/لل + name — the asker speaks next.
- **p10** amendment defense: يدافع عن (المقترح) + honorific + name.
- **p11** counter-speaker: والرأي ضد/المخالف + honorific + name (pairs with p10 in amendment debates).
- **p12** تفضلوا + السيد + name — polite plural used for ministers and officials.
- **p13/p14** يتولى/تتولى الدفاع constructions, both word orders ("ويتولى الدفاع عنه الأستاذة X", "وتتولى الأستاذة X الدفاع") — added from manual-review feedback.
- **p15** rhetorical question then bare name at line end ("من له رأي معارض؟ السيدة سامية عبو.") — the ؟ anchor keeps it out of running speech. Added from manual-review feedback.

**Manual-review incorporation (CorpusReview `reviews/`):** the human review of `2015-11-19_N_09` supplied 24 missed handoffs and 12 rejections. After the fixes above plus STOP-list additions (يطلب/تتولى/سيتولى… so "السيد X يطلب نقطة نظام" yields the bare name) and a **strict duration** requirement in p8 (numeral or possessive mandatory — kills roll-call false positives like "…لمدة عام"), all **24/24** review-marked handoffs are detected and the reviewed p8 false positive no longer fires. Corpus coverage after these changes: 80.4% / opener recall 95.3% (no regression).

**Giver→recipient resolution ("Sam to John"):** unchanged principle, now correct in practice — `find_last_handoff_cue` takes the **last** usable cue in a paragraph. In "thanks Sam … the floor goes to John", John is named last, so last-cue-wins picks the recipient. With p4 added, thanks-clauses are themselves cues, but positional ordering still resolves correctly.

### 3.2 Supporting regex changes

- `HONORIFICS` (used to strip titles from candidates): added المحترم/المحترمة variants ("للزميل المحترم السيد محسن العرفاوي").
- New shared blocks `HONOR_ANY` (honorifics with and without ال, incl. دكتور/سيدي) and `LIL_HONOR` (لل-prefixed forms).
- `STOP_AFTER_CANDIDATE` (trims trailing words off a captured name): added أربع/اربع/عشر/إحدى, لك/لديك, دقيقتين/دقيقه, and purpose clauses لتلاوة/لتقديم/لعرض/ليقدم/في حدود. Previously "صفية الخلفي أربع دقائق" kept "أربع" glued to the name.
- Candidate sanity filter in `find_last_handoff_cue`: candidates longer than **7 words** after honorific-stripping are rejected (real names, even with a role title, are short; long tails are cue verbs firing inside running speech). Bare imperative تفضلوا added to the self-referential skip set.

### 3.3 Boundary bug fix (silent window killer)

`is_boundary_text()` previously ran `VOTE_OR_ARTICLE.search(text[:250])`, so **any MP speech merely mentioning التصويت or "الفصل N" in its first 250 characters was classified as a boundary** — the capture window died and the speech was lost. Measured cost: ~100 lost speech-opener paragraphs per 60 files (96 of 104 boundary-related misses). Replaced with `VOTE_BOUNDARY`, a **start-anchored** set of chair formulas (نمر إلى التصويت، يعرض على التصويت، نصوت، تمت الموافقة، مقترح التعديل، الفصل + digit at paragraph start). `VOTE_OR_ARTICLE` is retained solely for `block_type` labeling.

### 3.4 Page-break stitching (`stitch_paragraphs`)

New pre-processing pass applied before cue detection in both `extract_turns` and `build_alias_table`:

1. Pure-noise paragraphs (page numbers, running headers, session-header lines) are dropped so they never separate a cue from its speech window.
2. A paragraph ending in a **dangling connector** — الكلمة, للسيد/للسيدة…, honorific, إلى, ثم, و — is merged with the next surviving paragraph. This repairs cues cut by PDF page breaks ("…الكلمة للسيد" ⏎ *page junk* ⏎ "فلان الفلاني").

### 3.5 Role-introduced speeches (`speaker_role` column)

Speeches introduced by function rather than name (وزير العدل، رئيس اللجنة، المقرر…) previously resolved to `speaker_type=office_or_role` with nothing actionable. Now the verbatim role string is preserved in a new CSV column `speaker_role` (empty for named MPs). This is the input for a downstream **LLM role-decoding pass** (role + session date → person, via government/committee rosters). Note from the 50-file sample: many `speaker_role` values are actually *name + role* ("محسن حسن رئيس كتلة الاتحاد الوطني الحر") — these could be resolved deterministically by stripping the role suffix before falling back to the LLM.

### 3.6 Misc

- `in_scope_files()` now uses `rglob` — the flat `glob` broke when the scripts moved into `Python scripts/` (corpus lives in `TN_democratic_cleaned_clean_md/`).
- The fuzzy-linking tiers, alias-table mining, and roster logic are untouched.

## 4. Generated data

- `gena_speech_assignments.csv` — full corpus run: **29,966 turns / 532 files**. 128 MB → **gitignored** (also exceeds GitHub's 100 MB hard limit). Rebuild: `python3 "Python scripts/handoff_window_assignment_gena.py" --all --output gena_speech_assignments.csv`.
- `gena_speech_assignments_50.csv` — 50-file sample (default seed 20260702): **2,529 turns / 49 files with turns**, 532 role-introduced. 11 MB, committed and pushed.
- Caveat for both: the MP roster CSV is not on this machine (`ROSTER_PATH` still points to Kevin's Dropbox), so **no rows link to `mp_id` yet** — everything is `unmatched_candidate`/`office_or_role` with `needs_review=1`. Set `TUNISIA_MP_ROSTER=/path/to/roster.csv` and re-run to populate MP identities, then regenerate `alias_table.csv` with `--build-aliases`.

## 5. Changes to `CorpusReview/server.py`

- **Data source switched to gena's determination.** The server now loads `gena_speech_assignments.csv` (override with env var `CORPUSREVIEW_GENA_CSV`) and derives both the `events` and `speeches` API payloads from it, including the new `speakerRole` field. Kevin's `deterministic_speech_assignments_2011_2021/*.csv` remain as a **fallback** when the gena CSV is absent. API shape is unchanged, so `static/app.js` needed no changes.
- **Corpus discovery fixed**: documents are found via a recursive basename→path index (`corpus_index()`), so the manifest lists all 534 files under `TN_democratic_cleaned_clean_md/`; `safe_root_file()` consults the index.
- **`.env` auto-loading**: a minimal dependency-free loader reads `<repo>/.env` at startup (`KEY=VALUE`, `#` comments; already-exported variables win). Translations therefore pick up `OPENAI_API_KEY` with no manual export.

Run: `python3 CorpusReview/server.py` → http://127.0.0.1:8765. Sample-CSV mode: `CORPUSREVIEW_GENA_CSV=gena_speech_assignments_50.csv python3 CorpusReview/server.py`.

## 6. Git & credentials

- **`.gitignore` created**: `.env`, `gena_speech_assignments.csv`, `test_corpus.csv`, `__pycache__/`, `.DS_Store`, `*.partial`.
- **Secret leak remediated**: `.env` (OpenAI key + GitHub token) had been committed in `90738dd` and pushed. Actions taken, at the owner's explicit instruction (keys **not** rotated, by owner's choice): `git rm --cached .env`; full history rewrite with `git filter-repo --path .env --invert-paths` (12 commits rewritten, `.env` now in zero commits); force-push to `origin/main`. Residual risk: pre-rewrite clones and GitHub's internal caches may still hold the old commits.
- **Push authentication**: a repo-local credential helper reads `GITHUB_TOKEN` from `.env` at push time; the token is never stored in `.git/config` or the remote URL.
- **Authorship incident & fix**: during the session a repo-local `user.email` was mistakenly set (to a gmail address), overriding the global GitHub-noreply identity on one commit. The local override was removed and the commit re-authored (`--reset-author`) and force-pushed. All history is now uniformly `GennadiiIakovlev <77676719+GennadiiIakovlev@users.noreply.github.com>`.
- **Other clones**: because of the two force-pushes, every other clone must adopt the rewritten history once — `git fetch origin && git reset --hard origin/main` (or rebase local commits onto `origin/main`). **Do not push from a stale clone**: it would reinstate the purged `.env` history.

## 7. Known limitations / next steps

1. **Remaining ~5% of missed openers**: mostly (a) bare anaphoric "تفضل" whose antecedent name sits several sentences back or in a previous paragraph, and (b) chair paragraphs that transfer the floor without naming anyone ("من يدافع عن هذا المقترح؟"). These need an alternation/adjacency model (classify each paragraph chair-vs-speech, attribute un-cued speech blocks by position) rather than more regex.
2. **Multi-speaker announcements** ("يدافع X … والرأي ضد Y") currently yield the last-named speaker only; the following *two* windows belong to X then Y. Window chaining is not implemented.
3. **p4 ambiguity**: `شكرا، NAME` at line end is treated as a call to NAME. The comma heuristic held up in spot-checks, but rows from p4 deserve review priority (filterable via `assignment_method`).
4. **LLM role decoding** (`speaker_role` → person) is designed but not implemented. Deterministic pre-step recommended: strip role suffixes and retry roster linking before invoking an LLM.
5. **Roster absent locally** — see §4; nothing links to `mp_id` until `TUNISIA_MP_ROSTER` is set.
6. Two files named `2009-2010_*` contain 2011 constituent-assembly content (mislabeled dates) and are excluded by the year filter; worth a filename audit.
