# Tunisian sitting-date validation — gate report

Total sittings: **548**

## Verdict distribution

| verdict | n |
|---|---|
| confirmed | 456 |
| single_witness | 6 |
| soft_flag | 21 |
| conflict | 49 |
| no_date | 16 |

## Per-gate coverage (pass / fail / na)

| gate | pass | fail | na |
|---|---|---|---|
| weekday | 361 | 38 | 149 |
| hijri | 1 | 0 | 547 |
| recurrence | 129 | 22 | 397 |
| marsad | 0 | 0 | 548 |
| sequence | 479 | 28 | 41 |
| collision | 532 | 0 | 16 |

Gates: **wk** weekday · **hj** hijri · **rc** recurrence · **ma** marsad · **sq** sequence · **co** collision (✓ pass, ✗ fail, · n/a). *suggest* = body-attested date when recurrence rejects the assigned one.

## Flagged sittings (70)

| stem | date | src | wk | hj | rc | ma | sq | co | verdict | suggest | evidence |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 2010-03-01__79 | 2010-03-01 | banner_recovered | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2010-07-05__28 | 2010-07-05 | filename | ✗ | · | ✗ | · | ✓ | ✓ | conflict | 2010-06-14 | weekday:الثلاثاء; recurrence:mode=2010-06-14x14 |
| 2011-11-19__seg040 | 2011-11-19 | banner_or_carried_forward | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الجمعه |
| 2014-01-01__seg041 | 2014-01-01 | banner_or_carried_forward | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2015-10-01__10 | 2015-10-01 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الخميس |
| 2016-03-30__48 | 2016-03-30 | banner_recovered | ✗ | · | ✓ | · | ✗ | ✓ | conflict |  | weekday:الجمعه; recurrence:mode=2016-03-30x3 |
| 2016-04-18__49 | 2016-04-18 | banner_recovered | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء; recurrence:mode=2017-04-18x48 |
| 2016-05-11__06 | 2016-05-11 | banner_recovered | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2016-06-21__25 | 2016-06-21 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2016-11-29__48 | 2016-11-29 | banner_recovered | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الاثنين; recurrence:mode=2017-03-31x4 |
| 2017-07-10__76 | 2017-07-10 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:السبت |
| 2018-03-22__50 | 2018-03-22 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الخميس |
| 2018-04-02__75 | 2018-04-02 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2018-04-03__76 | 2018-04-03 | filename_or_banner | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2018-05-17__49 | 2018-05-17 | banner_recovered | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الاثنين |
| 2018-06-22__80 | 2018-06-22 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الجمعه |
| 2018-10-29__29 | 2018-10-29 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الخميس |
| 2019-01-02__39 | 2019-01-02 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2019-03-01__68 | 2019-03-01 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الاربعاء |
| 2019-05-21__94 | 2019-05-21 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2019-06-12__100 | 2019-06-12 | filename_or_banner | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الجمعه |
| 2019-06-22__105 | 2019-06-22 | filename_or_banner | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2019-12-13__34 | 2019-12-13 | filename_or_banner | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2020-10-21__45 | 2020-10-21 | filename_or_banner | · | · | · | · | ✗ | ✓ | conflict |  | recurrence:mode=2020-10-28x2 |
| 2020-11-11__40 | 2020-11-11 | filename_or_banner | · | · | ✗ | · | ✗ | ✓ | conflict | 2019-11-29 | recurrence:mode=2019-11-29x3 |
| 2021-01-19__31 | 2021-01-19 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الثلاثاء |
| 2021-01-22__30 | 2021-01-22 | filename_or_banner | ✓ | · | · | · | ✗ | ✓ | conflict |  | weekday:الجمعه |
| 2021-03-18__46 | 2021-03-18 | banner_recovered | ✗ | · | · | · | ✗ | ✓ | conflict |  | weekday:الاثنين; recurrence:mode=2021-04-12x22 |
| 2023-07-31__21 | 2023-07-31 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2023-02-23 | recurrence:mode=2023-02-23x3 |
| 2023-10-17__03 | 2023-10-17 | filename_or_banner | ✓ | · | ✗ | · | ✓ | ✓ | conflict | 2022-07-01 | weekday:الثلاثاء; recurrence:mode=2022-07-01x3 |
| 2023-10-23__04 | 2023-10-23 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2023-06-23 | recurrence:mode=2023-06-23x4 |
| 2023-11-02__06 | 2023-11-02 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2023-07-06 | recurrence:mode=2023-07-06x4 |
| 2023-12-07__24 | 2023-12-07 | filename_or_banner | ✓ | · | ✗ | · | ✓ | ✓ | conflict | 2023-03-08 | weekday:الخميس; recurrence:mode=2023-03-08x3 |
| 2024-01-16__N29 | 2024-01-16 | multiday_first_day_min | ✓ | · | ✗ | · | · | ✓ | conflict | 2023-06-05 | weekday:الثلاثاء; recurrence:mode=2023-06-05x4 |
| 2024-02-06__N32 | 2024-02-06 | multiday_first_day_min | ✓ | · | ✗ | · | · | ✓ | conflict | 2024-01-16 | weekday:الثلاثاء; recurrence:mode=2024-01-16x7 |
| 2024-02-20__N34 | 2024-02-20 | multiday_first_day_min | ✓ | · | ✗ | · | · | ✓ | conflict | 2024-01-16 | weekday:الثلاثاء; recurrence:mode=2024-01-16x12 |
| 2024-02-27__35 | 2024-02-27 | filename_or_banner | ✓ | · | ✗ | · | ✓ | ✓ | conflict | 2023-06-13 | weekday:الثلاثاء; recurrence:mode=2023-06-13x4 |
| 2024-03-06__36 | 2024-03-06 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2024-02-16 | recurrence:mode=2024-02-16x8 |
| 2024-03-27__37 | 2024-03-27 | filename_or_banner | ✗ | · | ✗ | · | ✓ | ✓ | conflict | 2024-02-26 | weekday:الثلاثاء; recurrence:mode=2024-02-26x7 |
| 2024-05-07__38 | 2024-05-07 | filename_or_banner | ✓ | · | ✗ | · | ✓ | ✓ | conflict | 2024-03-04 | weekday:الثلاثاء; recurrence:mode=2024-03-04x3 |
| 2024-05-20__N39 | 2024-05-20 | multiday_first_day_min | ✗ | · | ✗ | · | · | ✓ | conflict | 2024-04-22 | weekday:الثلاثاء; recurrence:mode=2024-04-22x5 |
| 2024-06-04__N41 | 2024-06-04 | multiday_first_day_min | ✓ | · | ✗ | · | · | ✓ | conflict | 2024-05-14 | weekday:الثلاثاء; recurrence:mode=2024-05-14x3 |
| 2024-06-12__42 | 2024-06-12 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2024-05-20 | recurrence:mode=2024-05-20x3 |
| 2024-06-21__N43 | 2024-06-21 | multiday_first_day_min | ✗ | · | ✗ | · | · | ✓ | conflict | 2024-05-20 | weekday:الخميس; recurrence:mode=2024-05-20x9 |
| 2024-07-02__N45 | 2024-07-02 | multiday_first_day_min | · | · | ✗ | · | · | ✓ | conflict | 2024-04-25 | recurrence:mode=2024-04-25x3 |
| 2024-07-06__N46 | 2024-07-06 | multiday_first_day_min | · | · | ✗ | · | · | ✓ | conflict | 2024-05-15 | recurrence:mode=2024-05-15x3 |
| 2024-07-23__48 | 2024-07-23 | filename_or_banner | · | · | ✗ | · | ✓ | ✓ | conflict | 2023-10-01 | recurrence:mode=2023-10-01x5 |
| 2024-07-30__N49 | 2024-07-30 | multiday_first_day_min | ✓ | · | ✗ | · | · | ✓ | conflict | 2024-07-01 | weekday:الثلاثاء; recurrence:mode=2024-07-01x5 |
| 2025-06-20__26 | 2025-06-20 | filename_or_banner | · | · | · | · | ✗ | ✓ | conflict |  |  |
| 2011-01-17__28 | 2011-01-17 | banner_recovered | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الاربعاء |
| 2014-11-20__seg048 | 2014-11-20 | banner_or_carried_forward | ✗ | · | · | · | · | ✓ | soft_flag |  | weekday:الجمعه; recurrence:mode=2014-01-27x2 |
| 2015-05-31__31 | 2015-05-31 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الثلاثاء; recurrence:mode=2015-06-02x2 |
| 2015-06-10__33 | 2015-06-10 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الثلاثاء |
| 2016-03-23__42 | 2016-03-23 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:السبت |
| 2017-07-27__95 | 2017-07-27 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
| 2017-09-20__03 | 2017-09-20 | filename_or_banner | ✗ | · | ✓ | · | ✓ | ✓ | soft_flag |  | weekday:السبت; recurrence:mode=2017-09-20x2 |
| 2018-10-08__1 | 2018-10-08 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الثلاثاء |
| 2019-04-11__81 | 2019-04-11 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الاثنين |
| 2019-04-28__86 | 2019-04-28 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
| 2020-02-24__47 | 2020-02-24 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الخميس |
| 2020-09-22__11 | 2020-09-22 | filename_or_banner | ✗ | · | ✓ | · | · | ✓ | soft_flag |  | weekday:الجمعه; recurrence:mode=2020-09-22x3 |
| 2020-09-24__1 | 2020-09-24 | filename_or_banner | ✗ | · | · | · | · | ✓ | soft_flag |  | weekday:الجمعه |
| 2020-10-08__2 | 2020-10-08 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الثلاثاء |
| 2021-02-01__28 | 2021-02-01 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
| 2021-03-01__37 | 2021-03-01 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الخميس |
| 2021-03-04__41 | 2021-03-04 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
| 2023-03-31__01 | 2023-03-31 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الاثنين |
| 2023-04-29__09 | 2023-04-29 | filename_or_banner | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
| 2024-12-09__28 | 2024-12-09 | filename | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الخميس |
| 2024-12-31__31 | 2024-12-31 | filename | ✗ | · | · | · | ✓ | ✓ | soft_flag |  | weekday:الجمعه |
