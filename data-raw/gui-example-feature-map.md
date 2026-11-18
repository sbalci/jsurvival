# Explore the saved jsurvival examples

Open a workbook below in jamovi, then select its analysis in the results pane to
see the selected variables and settings. Read the teaching note and the analysis
explanations before changing options. Save your experiments under a new filename
to keep the supplied starting point available.

The ten workbooks contain thirteen saved analyses covering all nine functions.
All observations are invented. The survival cohorts use months, with `event = 1`
for an observed event and `event = 0` for right censoring. Group names and model
associations are illustrative. These examples show selected workflows; they do
not demonstrate every option combination.

| Function and workbook | Saved key features | Try next: settings not saved in this workbook |
| --- | --- | --- |
| [Single Arm Survival](../data/singlearm_example.omv) | Whole-cohort Kaplan–Meier curve, confidence intervals, risk table, censor marks, survival estimates and person-time rates. | Compare cumulative event probability and cumulative hazard. Try a landmark time and inspect how the eligible cohort changes. |
| [Survival Analysis](../data/survival_example.omv) | Group comparison, age adjustment, adjusted curves, proportional-hazards diagnostics, RMST at an explicit 24-month horizon, summaries, explanations and REMARK reporting guidance. | Choose another prespecified RMST horizon supported by both groups, or inspect residual and log-log diagnostics. |
| [Continuous Marker Survival](../data/survivalcont_example.omv) | Continuous-marker Cox model, an exploratory optimal cut-off, grouped Kaplan–Meier curves and an exported cut-off group. | Try multiple cut-offs using the quantile method and compare the resulting group sizes. A data-derived cut-off needs independent validation before clinical use. |
| [Multivariable Survival](../data/multisurvival_example.omv) | Cox model with categorical `group` and `stage`, continuous `age` and `marker`, a hazard-ratio forest plot and proportional-hazards diagnostics. | Enable the adjusted probability curve and select its adjustment method explicitly. Explore risk scores and risk-group plots as development-sample descriptions. The saved model has no interaction terms. |
| [LASSO Cox](../data/lassocox_example.omv) | Two analyses compare minimum-error and 1-SE penalty rules with five CV folds, seed 123456 and standardization. Each includes four plots, encoding/reproducibility details, reproducible R code, explanations and an exported development risk score. | Change the seed separately to explore selection stability, or enable variable importance and exploratory Cox comparisons. |
| [Odds Ratios: Firth safeguard](../data/oddsratio_example.omv) | Firth logistic regression, an odds-ratio table and forest plot, plus the explanation that a Firth nomogram is unavailable. | Turn off Firth to explore an ordinary logistic fit. Diagnostic metrics additionally require a selected binary diagnostic predictor and both predictor levels. Use the separate pathology workbook below for a coherent diagnostic example. |
| [Odds Ratios: binary pathology](../data/oddsratio_binary_example.omv) | Ordinary logistic regression of nodal involvement at surgery on age, tumour size and grade. An explicitly coded, separate diagnostic test supplies the 2×2 table and diagnostic metrics; the fitted model has a prediction nomogram. | Add `diagnostic_test` as a regression predictor to compare its adjusted association with its separate unadjusted diagnostic performance, or compare models with and without grade. |
| [DateTime Converter](../data/datetimeconverter_example.omv) | ISO dates with explicit YMD format and UTC, year/month/day extraction and an exported year; one source date is missing. | Select `excel_date` and its Excel serial format. Select `mixed_date` to inspect a malformed value, with the data quality assessment enabled. Check the converted dates against the original text. |
| [Outcome Organizer](../data/outcomeorganizer_example.omv) | Two analyses and exported outcomes compare overall survival with competing risks. Both death categories are overall-survival events; competing risks retain disease deaths, other-cause deaths and censoring separately. Diagnostics, visualizations, summaries and glossary are included. | Explore cause-specific recoding and compare its endpoint definition with the saved competing-risk representation before selecting a downstream analysis. |
| [Time Intervals](../data/timeinterval_example.omv) | Two analyses and exported columns compare standardized months (elapsed days / 30.4375) with calendar-aware months. Both include quality assessment, summary and glossary. | Switch output units to days to verify date subtraction, or examine a landmark setting and the resulting excluded records and time origin. |

## Read the results in context

- LASSO's saved risk groups describe its development data. Their apparent
  separation does not establish predictive performance in new patients. The
  multivariable example fits associations without saved risk-group output.
- The Firth example's outcome records whether an event was observed during varying
  follow-up. Its odds ratios are binary associations, not fixed-horizon survival
  risks or diagnostic accuracy estimates. The separate 240-patient binary pathology
  workbook uses nodal involvement at surgery and an explicitly coded test result.
  Its diagnostic counts are 79 true positives, 20 false positives, 12 false
  negatives and 129 true negatives.
- Outcome Organizer prepares event codes. A composite endpoint also needs the
  time to its first qualifying event; recoding alone does not supply that time.
- In the DateTime example, some missing extracted components may display `NaN`
  in the preview. The exported year has a genuine missing value. See
  [the workbook guide](../GUI_EXAMPLES.md) for verification details and limits.

For the dataset descriptions, event counts and maintenance instructions, see
[Saved jamovi examples](../GUI_EXAMPLES.md). The last column suggests additional
settings to explore beyond each workbook's saved analyses.
