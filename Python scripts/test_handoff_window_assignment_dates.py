#!/usr/bin/env python3
"""Tests for handoff_window_assignment_dates.py — pure helpers + a synthetic pipeline run."""
import csv
from datetime import date

import pytest

import handoff_window_assignment_dates as m


# ------------------------------------------------------------------ normalisation
def test_norm_strips_tashkeel_and_folds():
    assert m.norm("مُحَرَّم") == "محرم"
    assert m.norm("١٧") == "17"
    assert m.norm("أإآ") == "ااا"
    assert m.norm("جمادى") == "جمادي"
    assert m.norm("الجمعة") == "الجمعه"


def test_month_keys_are_prefolded():
    # every calendar key must already be in folded form, or norm() will never match it
    for key in (*m.GREG_MONTHS, *m.WEEKDAYS, *m.HIJRI_MONTHS):
        assert m.norm(key) == key, key


# ------------------------------------------------------------------ date helpers
def test_parse_iso_and_make_date():
    assert m.parse_iso("2017-12-07") == date(2017, 12, 7)
    assert m.parse_iso("") is None
    assert m.parse_iso("garbage") is None
    assert m.make_date(2017, 13, 40) is None      # invalid month/day
    assert m.make_date("2017", "12", "07") == date(2017, 12, 7)


@pytest.mark.parametrize("hy,hm,hd,greg", [
    (1379, 5, 17, date(1959, 11, 18)),   # anchors from the corpus
    (1400, 1, 1, date(1979, 11, 21)),
])
def test_hijri_conversion_within_tolerance(hy, hm, hd, greg):
    g = m.hijri_to_greg(hy, hm, hd)
    assert g is not None and abs((g - greg).days) <= m.HIJRI_TOL


def test_period_bounds():
    assert m.period_bounds("2017-2018") == (date(2017, 9, 1), date(2018, 8, 31))
    assert m.period_bounds("") == (None, None)


# ------------------------------------------------------------------ sequence / band
def test_lis_backbone_keeps_monotone_run():
    items = [("a", date(2017, 1, 1)), ("b", date(2017, 3, 1)),
             ("c", date(2016, 1, 1)), ("d", date(2017, 4, 1))]
    bb = m.lis_backbone(items)
    assert bb == {"a", "b", "d"}          # c is the out-of-order break
    assert m.lis_backbone([]) == set()


def test_ref_band_and_in_band():
    band = m.ref_band("2017-12-07__3", "2017-12-07__3.md")
    assert band == (2016, 2018)
    assert m.in_band(date(2018, 1, 4), band)
    assert not m.in_band(date(2023, 1, 1), band)      # stray OCR year
    assert m.in_band(date(1900, 1, 1), None)          # no tokens -> no guard


# ------------------------------------------------------------------ stem parsing
def test_stem_period_seq_covers_corpus_shapes():
    # date stem: parliamentary year runs Sep..Aug
    assert m.stem_period_seq("2014-05-06__31") == ("2013-2014", 31, "num")
    assert m.stem_period_seq("2014-10-06__2") == ("2014-2015", 2, "num")
    # undated session-range stem: period taken verbatim
    assert m.stem_period_seq("2016-2017__03") == ("2016-2017", 3, "num")
    # NCA segment stems get their own series (seg numbering != sitting numbering)
    assert m.stem_period_seq("2011-11-19__seg040") == ("2011-2012", 40, "seg")
    # prose suffixes: period known, no sequence number
    assert m.stem_period_seq("2011-12-13__Session_Ordinaire") == ("2011-2012", None, "num")
    # trailing prose after the sitting number is tolerated
    assert m.stem_period_seq("2014-01-26__14_Revision_de_la_loi_electorale") == ("2013-2014", 14, "num")
    assert m.stem_period_seq("garbage") == ("", None, "")


def test_load_marsad_missing_file_disables_gate(tmp_path):
    assert m.load_marsad(tmp_path / "absent.txt") == set()


# ------------------------------------------------------------------ scanning
def test_scan_dates_arabic_and_numeric_with_band():
    lines = ["افتتحت الجلسة يوم ٧ ديسمبر ٢٠١٧", "محضر بتاريخ 14/12/2017", "رقم 2099 غير تاريخ"]
    got = m.scan_dates(lines, band=(2016, 2019))
    assert date(2017, 12, 7) in got
    assert date(2017, 12, 14) in got
    assert all(d.year <= 2019 for d in got)           # 2099 filtered out


# ------------------------------------------------------------------ verdict logic
def test_verdict_of_covers_each_branch():
    P, F, N = "pass", "fail", "na"
    def g(rc=N, sq=N, co=N, wk=N, hj=N, ma=N):
        return {"recurrence": rc, "sequence": sq, "collision": co,
                "weekday": wk, "hijri": hj, "marsad": ma}
    fd = date(2017, 12, 7)
    assert m.verdict_of(None, g()) == "no_date"
    assert m.verdict_of(fd, g(rc=F)) == "conflict"                 # hard fail
    assert m.verdict_of(fd, g(ma=F)) == "conflict"                 # soft fail, no support
    assert m.verdict_of(fd, g(ma=F, sq=P)) == "soft_flag"          # soft fail, corroborated
    assert m.verdict_of(fd, g(rc=P, sq=P)) == "confirmed"          # 2 passes, 0 fail
    assert m.verdict_of(fd, g(sq=P)) == "single_witness"
    assert m.verdict_of(fd, g()) == "unverifiable"


# ------------------------------------------------------------------ doc gates
def _weekday_word(d):
    rev = {}
    for word, wd in m.WEEKDAYS.items():
        rev.setdefault(wd, word)
    return rev[d.weekday()]


def test_doc_gates_weekday_and_recurrence(tmp_path):
    fd = date(2017, 12, 7)
    body = f"يوم {_weekday_word(fd)} ٧ ديسمبر ٢٠١٧\n" + "الجلسة ٧ ديسمبر ٢٠١٧\n" * 10
    f = tmp_path / "s.md"
    f.write_text(body, encoding="utf-8")
    wk, hj, rc, ev = m.doc_gates(f, fd, m.ref_band("2017-12-07__1", "s.md"))
    assert wk == "pass"
    assert rc == "pass"
    assert "recurrence" in ev


def test_doc_gates_recurrence_fail_suggests_body_date(tmp_path):
    fd = date(2017, 12, 28)                      # assigned date the body never states
    body = "الجلسة ٤ جانفي ٢٠١٨\n" * 4           # body insists on Jan 4 2018
    f = tmp_path / "s.md"
    f.write_text(body, encoding="utf-8")
    wk, hj, rc, ev = m.doc_gates(f, fd, m.ref_band("2017-12-28__4", "s.md"))
    assert rc == "fail"
    assert ev["_suggest"] == "2018-01-04"


# ------------------------------------------------------------------ end-to-end
def _paths(tmp_path):
    out = tmp_path / "out"
    out.mkdir()
    return m.Paths(out=out, manifest=out / "manifest.csv",
                   marsad=out / "marsad.txt", search_dirs=(out,))


def test_full_pipeline(tmp_path):
    p = _paths(tmp_path)
    # 3 clean sequential sittings + 1 recurrence-conflict + 1 undated range file
    dates = {"2017-12-07__1": date(2017, 12, 7), "2017-12-14__2": date(2017, 12, 14),
             "2017-12-21__3": date(2017, 12, 21), "2017-12-28__4": date(2017, 12, 28)}
    p.marsad.write_text("\n".join(d.isoformat() for d in dates.values()) + "\n")

    rows = []
    for stem, d in dates.items():
        src = f"{stem}.md"
        rows.append({"doc_id": stem, "md_file": f"md/{src}",
                     "sitting_date": d.isoformat(), "date_source": "filename_or_banner"})
        if stem == "2017-12-28__4":               # body contradicts the assigned date
            body = "الجلسة ٤ جانفي ٢٠١٨\n" * 4
        else:
            body = f"يوم {_weekday_word(d)} {d.day} ديسمبر ٢٠١٧\n" + \
                   f"الجلسة {d.day} ديسمبر ٢٠١٧\n" * 8
        (p.out / src).write_text(body, encoding="utf-8")
    rows.append({"doc_id": "2018-2019__1", "md_file": "md/2018-2019__1.md",
                 "sitting_date": "", "date_source": "session_range"})
    (p.out / "2018-2019__1.md").write_text("لا يوجد تاريخ\n", encoding="utf-8")
    m.write_rows(p.manifest, ["doc_id", "md_file", "sitting_date", "date_source"], rows)

    m.run_all(p)

    gated = {r["session_stem"]: r for r in
             csv.DictReader(p.gated.open(newline="", encoding="utf-8"))}
    assert gated["2017-12-07__1"]["verdict"] == "confirmed"
    assert gated["2017-12-28__4"]["verdict"] == "conflict"
    assert gated["2017-12-28__4"]["suggested_date"] == "2018-01-04"
    assert gated["2018-2019__1"]["verdict"] == "no_date"
    assert p.report.exists() and "gate report" in p.report.read_text()


def test_full_pipeline_without_marsad_calendar(tmp_path):
    """Marsad file absent on this machine: gate must degrade to na, not fail."""
    p = _paths(tmp_path)                          # marsad.txt never written
    d = date(2017, 12, 7)
    (p.out / "2017-12-07__1.md").write_text(
        f"يوم {_weekday_word(d)} ٧ ديسمبر ٢٠١٧\n" + "الجلسة ٧ ديسمبر ٢٠١٧\n" * 8,
        encoding="utf-8")
    m.write_rows(p.manifest, ["doc_id", "md_file", "sitting_date", "date_source"],
                 [{"doc_id": "2017-12-07__1", "md_file": "md/2017-12-07__1.md",
                   "sitting_date": d.isoformat(), "date_source": "filename_or_banner"}])
    m.run_all(p)
    row = next(csv.DictReader(p.gated.open(newline="", encoding="utf-8")))
    assert row["marsad"] == "na"
    assert row["verdict"] == "confirmed"          # weekday + recurrence still corroborate


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v"]))
