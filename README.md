# GGIR Post Review

A post-processing and quality-control toolkit for GGIR outputs. The goal is to help researchers organize GGIR runs, audit parameter choices, combine output files, flag suspicious sleep metrics, identify candidate secondary sleep / nap periods, and generate a transparent review report.

> Status: initial design spec. This repository is intended to become an R package / CLI workflow that runs after a completed GGIR analysis.

## Why this tool exists

GGIR is powerful, but post-run review still requires many manual steps:

1. checking which GGIR parameters were changed from defaults;
2. combining participant-level and night-level output files;
3. reviewing sleep duration, SPT, WASO, sleep efficiency, non-wear, light, and temperature patterns;
4. identifying suspicious nights that need manual review;
5. handling split sleep or nap-like periods that may not be represented well by a single main sleep period;
6. deciding when a better sleep diary or manual adjudication is needed;
7. creating a reproducible report that documents all decisions.

This project is designed to sit outside GGIR, read GGIR output folders, and produce review-ready outputs without modifying the original GGIR results.

## Proposed package name

Working name: `ggirpostreview`

Alternative names:

- `GGIRPostReview`
- `ActiSleepQC`
- `SleepReviewR`
- `GGIRSleepAudit`

## Core principles

- Keep original GGIR files read-only.
- Prefer CSV / epoch-level data over PDF image parsing.
- Use deterministic rule-based QC first.
- Keep all AI/model-assisted decisions explainable and auditable.
- Separate automatic flags from final human decisions.
- Store every threshold and rule in a versioned configuration file.
- Produce both machine-readable outputs and human-readable reports.

## Main workflow

```r
library(ggirpostreview)

review <- run_ggir_post_review(
  ggir_output_dir = "path/to/GGIR/output",
  study_dates_file = "path/to/study_dates.csv",
  sleep_diary_file = "path/to/sleep_diary.csv",
  config_file = "post_review.yml",
  out_dir = "ggir_post_review_outputs"
)
```

Expected outputs:

- `parameter_audit.csv`
- `combined_nights.csv`
- `combined_days.csv`
- `subject_qc_summary.csv`
- `night_flags.csv`
- `sleep_candidates.csv`
- `manual_review_queue.csv`
- `final_sleep_periods_template.csv`
- `ggir_post_review_report.html`
- optional `ggir_post_review_report.pdf`

## Module 1: GGIR run organizer

Purpose: detect and organize relevant GGIR output files.

Responsibilities:

- locate GGIR result folders;
- identify Part 2 / Part 4 / Part 5 output files;
- identify participant-level, day-level, night-level, and epoch-level files;
- standardize subject IDs, calendar dates, nights, and time zones;
- preserve paths to original PDFs and plots;
- create a run manifest.

Output:

```text
run_manifest.csv
```

Suggested columns:

- `run_id`
- `ggir_version`
- `output_dir`
- `file_type`
- `file_path`
- `subject_id`
- `part`
- `created_time`
- `hash`

## Module 2: Parameter audit

Purpose: compare the parameters used in a GGIR run against version-specific defaults.

Important design note: GGIR defaults can change by package version, so this tool should maintain a default registry by GGIR version. If the exact version is unavailable, the tool should mark the audit as approximate instead of silently assuming defaults.

Output:

```text
parameter_audit.csv
```

Suggested columns:

- `parameter`
- `default_value`
- `run_value`
- `changed`
- `impact_area`
- `impact_level`
- `comment`

Suggested `impact_area` values:

- `path_or_io`
- `calibration`
- `nonwear`
- `sleep_detection`
- `sustained_inactivity`
- `day_boundary`
- `time_zone`
- `reporting`
- `unknown`

Suggested `impact_level` values:

- `low`
- `medium`
- `high`

High-impact examples include parameters that affect time zone handling, sleep detection, non-wear detection, sustained inactivity bout detection, study day boundaries, and the use of sleep logs.

## Module 3: Combined GGIR outputs

Purpose: combine key GGIR output files into consistent study-level datasets.

Outputs:

```text
combined_nights.csv
combined_days.csv
combined_subject_summary.csv
```

The combined night dataset should contain one row per subject-night, including:

- `subject_id`
- `night_date`
- `calendar_date`
- `ggir_sleep_onset`
- `ggir_wakeup`
- `spt_duration_hours`
- `sleep_duration_hours`
- `waso_minutes`
- `sleep_efficiency`
- `nonwear_fraction_spt`
- `light_mean_spt`
- `light_max_spt`
- `temperature_mean_spt`
- `temperature_min_spt`
- `diary_bedtime`
- `diary_waketime`
- `diary_available`
- `source_file`

## Module 4: Sleep metric QC rules

Purpose: automatically flag suspicious nights and subject-level patterns.

Suggested night-level flags:

| Flag | Trigger |
|---|---|
| `missing_main_sleep` | no main sleep period detected |
| `very_short_spt` | SPT duration < 2 hours |
| `very_long_spt` | SPT duration > 14 hours |
| `very_short_sleep` | sleep duration < 1 hour |
| `very_long_sleep` | sleep duration > 12 hours |
| `low_sleep_efficiency` | sleep efficiency < 65% |
| `implausibly_high_sleep_efficiency` | sleep efficiency > 98% with low movement variability |
| `high_waso` | WASO > 180 minutes |
| `high_nonwear_in_spt` | non-wear fraction in SPT > 20% |
| `sleep_outside_expected_window` | onset/wake far from diary or study window |
| `possible_split_sleep` | two or more sleep-like inactive periods in one noon-to-noon window |
| `possible_daytime_sleep` | sleep-like period occurs outside main nocturnal window |
| `light_inconsistent_with_sleep` | sustained high light during reported sleep |
| `temperature_inconsistent_with_wear` | temperature suggests device not worn or unstable |
| `duplicate_or_overlapping_night` | duplicate dates or overlapping sleep windows |
| `timezone_or_dst_warning` | unusual time shift or daylight saving issue |

Suggested subject-level flags:

| Flag | Trigger |
|---|---|
| `too_few_valid_nights` | fewer than study-required valid nights |
| `many_flagged_nights` | >30% nights flagged |
| `systematic_diary_missingness` | diary missing for most nights |
| `high_night_to_night_variability` | unusually large variation in sleep timing/duration |
| `repeated_boundary_problem` | repeated onset/wake near noon-to-noon boundary |
| `possible_protocol_nonadherence` | patterns suggest device removed or wrong wear schedule |

## Module 5: Better sleep diary suggestion engine

Purpose: translate QC flags into researcher-facing recommendations.

Example recommendation rules:

- If multiple nights have `possible_split_sleep`, recommend confirming naps / second sleep periods in the diary.
- If `sleep_outside_expected_window` occurs often, recommend checking AM/PM entries and date alignment.
- If light is high during reported sleep, recommend confirming whether the device was worn during sleep or exposed to room light.
- If temperature is low or unstable during SPT, recommend checking non-wear and device placement.
- If diary is missing and automated sleep timing is unstable, recommend manual review before final derivation.

Output:

```text
review_recommendations.csv
```

Suggested columns:

- `subject_id`
- `night_date`
- `recommendation_type`
- `recommendation_text`
- `supporting_flags`
- `priority`

## Module 6: Candidate second sleep / nap detection

Purpose: identify additional sleep-like periods that GGIR may not represent as the main sleep period.

Preferred input: epoch-level time series with at least:

- timestamp;
- ENMO or activity metric;
- angle / anglez or posture proxy;
- light;
- temperature;
- non-wear indicator if available;
- GGIR-derived sustained inactivity bout labels if available.

Important note: if the available Part 5 CSV only contains night-level summaries, this module cannot reliably detect second sleep periods. It needs epoch-level or bout-level information.

### Candidate detection approach

1. Build noon-to-noon or study-defined day windows.
2. Identify sustained low-activity periods using ENMO and angle variability.
3. Exclude likely non-wear using temperature, non-wear labels, and extreme inactivity patterns.
4. Score each candidate period using:
   - duration;
   - low activity;
   - low angle variability;
   - low light;
   - stable or sleep-compatible temperature;
   - time-of-day prior;
   - diary support;
   - proximity to existing GGIR main sleep.
5. Classify each candidate as:
   - `main_sleep`
   - `secondary_sleep`
   - `nap_candidate`
   - `quiet_wake_candidate`
   - `nonwear_candidate`
6. Keep confidence scores and reasons.

Suggested output:

```text
sleep_candidates.csv
```

Suggested columns:

- `subject_id`
- `window_date`
- `candidate_id`
- `start_time`
- `end_time`
- `duration_minutes`
- `candidate_type`
- `confidence`
- `mean_enmo`
- `sd_anglez`
- `mean_light`
- `mean_temperature`
- `nonwear_fraction`
- `diary_overlap`
- `ggir_main_sleep_overlap`
- `reason_codes`

## Module 7: Model-assisted sleep/wake classification

Recommended staged approach:

### Version 0: deterministic rules

Use transparent thresholds and scoring rules. This is the safest starting point for research workflows.

### Version 1: interpretable model

Use logistic regression, decision tree, or generalized additive model trained on reviewed nights or PSG/diary-supported labels.

Candidate features:

- ENMO mean / median / rolling SD;
- anglez rolling SD;
- light mean / max;
- temperature mean / slope;
- time since noon;
- clock time;
- day/night indicator;
- nonwear probability;
- diary overlap;
- previous and next epoch state.

### Version 2: sequence model

Use hidden Markov model or another temporal model to smooth sleep/wake states and avoid fragmented predictions.

Research recommendation: avoid a black-box LLM as the primary classifier. Use AI/LLM only for summarizing evidence and generating review explanations. The actual sleep/wake classification should be reproducible, versioned, and inspectable.

## Module 8: PDF / visual review

Purpose: support human review of suspicious nights.

Preferred approach:

- recreate night plots from GGIR CSV / epoch data;
- overlay GGIR main sleep, candidate secondary sleep, diary bed/wake, non-wear, light, and temperature;
- generate one page per subject-night;
- create a review queue sorted by priority.

PDF image parsing should be optional only. It is less reliable than rebuilding plots from data.

Suggested review page elements:

- activity timeline;
- light timeline;
- temperature timeline;
- GGIR sleep window;
- candidate second sleep / nap window;
- diary window;
- flags and reason codes;
- manual reviewer decision fields.

Manual decision template:

```text
manual_review_decisions.csv
```

Suggested columns:

- `subject_id`
- `night_date`
- `reviewer`
- `decision`
- `final_sleep_onset`
- `final_wakeup`
- `final_secondary_sleep_onset`
- `final_secondary_wakeup`
- `include_secondary_sleep_in_total`
- `notes`

## Proposed R package structure

```text
ggir-post-review/
  DESCRIPTION
  NAMESPACE
  README.md
  R/
    run_ggir_post_review.R
    discover_ggir_outputs.R
    audit_ggir_parameters.R
    combine_ggir_outputs.R
    apply_sleep_qc_rules.R
    detect_sleep_candidates.R
    score_sleep_candidates.R
    build_review_queue.R
    render_review_report.R
    utils_dates.R
    utils_files.R
  inst/
    defaults/
      ggir_defaults_template.yml
    rmarkdown/
      templates/
        ggir-post-review-report/
          skeleton.Rmd
  tests/
    testthat/
      test-parameter-audit.R
      test-qc-rules.R
      test-sleep-candidates.R
  vignettes/
    getting-started.Rmd
  post_review.yml
```

## Proposed configuration file

```yaml
study:
  timezone: "America/New_York"
  day_window: "noon_to_noon"
  min_valid_nights: 3

qc_thresholds:
  very_short_spt_hours: 2
  very_long_spt_hours: 14
  very_short_sleep_hours: 1
  very_long_sleep_hours: 12
  low_sleep_efficiency_pct: 65
  high_sleep_efficiency_pct: 98
  high_waso_minutes: 180
  high_nonwear_fraction_spt: 0.20

candidate_sleep:
  min_duration_minutes: 30
  merge_gap_minutes: 20
  min_confidence_for_review: 0.60
  include_daytime_naps: true

review:
  generate_html: true
  generate_pdf: false
  include_only_flagged_nights: false
```

## Minimal viable product

MVP 1 should include:

1. file discovery and run manifest;
2. combined night summary;
3. parameter audit framework;
4. rule-based sleep QC flags;
5. recommendation table;
6. HTML report;
7. manual review queue.

MVP 2 should add:

1. epoch-level candidate second sleep / nap detection;
2. visual overlays;
3. manual decision import;
4. final adjudicated sleep period export.

MVP 3 should add:

1. model-assisted sleep/wake classification;
2. reviewed-label training dataset support;
3. validation report against diary or PSG labels;
4. Shiny reviewer interface.

## Important limitation

This tool can improve review efficiency and reproducibility, but it should not silently replace human sleep adjudication in research studies. Any derived second sleep / nap periods should include confidence, reason codes, and manual review status.
