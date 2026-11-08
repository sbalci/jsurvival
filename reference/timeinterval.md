# Comprehensive Time Interval Calculator

Advanced time interval calculator designed for survival analysis,
epidemiological studies, and person-time analysis. Features intelligent
date parsing, comprehensive data quality assessment, landmark analysis,
and robust statistical summaries. Time intervals form the foundation of
person-time follow-up calculations, capturing both participant counts
and observation duration for accurate incidence rate calculations.

## Usage

``` r
timeinterval(
  data,
  dx_date = NULL,
  fu_date = NULL,
  time_format = "auto",
  output_unit = "months",
  time_basis = "calendar",
  use_landmark = FALSE,
  landmark_time = 6,
  remove_negative = FALSE,
  remove_extreme = FALSE,
  extreme_multiplier = 2,
  include_quality_metrics = FALSE,
  confidence_level = 95,
  show_summary = FALSE,
  show_glossary = FALSE,
  timezone = "system"
)
```

## Arguments

- data:

  The data as a data frame containing date columns for interval
  calculation.

- dx_date:

  Column containing start dates (e.g., diagnosis date, study entry,
  treatment start). Accepts text dates and numeric packed dates
  (YYYYMMDD, YYMMDD). Spreadsheet day-count serial numbers (Excel day
  counts such as 42370) are not dates and are rejected, because parsing
  them gives follow-up times wrong by decades; re-export that column as
  text in YYYY-MM-DD form.

- fu_date:

  Column containing end dates (e.g., follow-up date, event date, study
  exit). Must be in the same format as the start date variable.
  Spreadsheet day-count serial numbers are rejected for the same reason
  as the start date column.

- time_format:

  Date format specification. 'Auto-detect' identifies the format from
  the shape of the text, including day-first and month-first dates with
  a time of day, and year-month dates, which have no day and are set to
  the 1st of the month with a warning. Manual selection ensures accurate
  parsing for specific date formats; a value that cannot have the
  selected format is left empty. Two-digit years are read as 2000-2068
  for 00-68 and as 1969-1999 for 69-99; a message reports how many
  values were parsed that way.

- output_unit:

  Unit for calculated time intervals. Affects person-time calculations
  and statistical summaries. Choose based on study duration and event
  frequency.

- time_basis:

  Controls how months/years are computed. Calendar-aware (the default)
  respects actual month lengths (28 - 31 days), as the survival analyses
  in this module do when they compute time from dates, so the same dates
  give the same months. Standardized uses fixed lengths (30.4375 days
  per month, 365.25 days per year), a constant unit for person-time
  denominators.

- use_landmark:

  Enables conditional analysis from a specific time point. Useful for
  studying outcomes conditional on having been followed to a landmark
  time (for example, only participants with at least 6 months of
  follow-up).

- landmark_time:

  Time point for landmark analysis in the specified output units. Only
  participants whose follow-up reaches this time are included. This
  analysis has no event indicator, so it selects on LENGTH OF FOLLOW-UP,
  not on survival: a participant excluded here may have died early or
  may simply have been enrolled recently, and this analysis cannot tell
  the two apart.

- remove_negative:

  Automatically exclude negative time intervals (end date before start
  date). Recommended for data quality assurance.

- remove_extreme:

  Exclude time intervals above (multiplier x 99th percentile) from the
  analysis. These rows are dropped from every statistic, including the
  mean and the total person-time, not merely flagged. The Data Quality
  Assessment panel reports how many were removed. Leave off to keep all
  intervals and only see the count. With fewer than 101 intervals the
  99th percentile is interpolated between the two longest intervals, so
  the rule can remove at most the single longest one, and none at all
  when n is 51 or fewer at the default multiplier of 2; the analysis
  says so when that applies.

- extreme_multiplier:

  Multiplier for 99th percentile to define extreme values. Default 2.0
  means intervals longer than 2x the 99th percentile are removed when
  'Remove extreme values' is on. Higher values are more conservative
  (fewer intervals removed). Has no effect when the 99th percentile is
  zero.

- include_quality_metrics:

  Provides comprehensive data quality assessment including missing
  values, negative intervals, and distribution statistics.

- confidence_level:

  Confidence level for statistical intervals (mean confidence
  intervals). Standard epidemiological practice uses 95 percent
  confidence intervals.

- show_summary:

  Generate a plain-language interpretation of results suitable for
  copying into reports or clinical notes.

- show_glossary:

  Display definitions of key terms (person-time, incidence rate,
  landmark analysis, etc.) to help interpret results.

- timezone:

  Timezone for datetime parsing. 'System Default' uses your computer's
  timezone. 'UTC' interprets datetimes as Coordinated Universal Time.
  This applies only to values carrying a time of day; date-only formats
  produce calendar dates that have no timezone, so the setting has no
  effect on them. Note that it changes the reported DURATIONS as well as
  the parsing: a zone with daylight saving gains or loses an hour at
  each transition, so an interval spanning one comes out an hour short
  under 'System Default' and exact under 'UTC'. Choose UTC if you need
  durations unaffected by daylight saving.

## Value

A results object containing:

|                             |     |     |     |     |           |
|-----------------------------|-----|-----|-----|-----|-----------|
| `results$messages`          |     |     |     |     | a html    |
| `results$todo`              |     |     |     |     | a html    |
| `results$aboutPanel`        |     |     |     |     | a html    |
| `results$personTimeInfo`    |     |     |     |     | a html    |
| `results$qualityCounts`     |     |     |     |     | a table   |
| `results$qualityAssessment` |     |     |     |     | a html    |
| `results$caveatsPanel`      |     |     |     |     | a html    |
| `results$summary`           |     |     |     |     | a html    |
| `results$nlSummary`         |     |     |     |     | a html    |
| `results$glossaryPanel`     |     |     |     |     | a html    |
| `results$calculated_time`   |     |     |     |     | an output |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$qualityCounts$asDF`

`as.data.frame(results$qualityCounts)`
