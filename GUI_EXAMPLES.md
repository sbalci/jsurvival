# Saved jamovi examples

All nine module analyses have a synthetic example in `data/`. Analyses
were added, configured and saved through the jamovi GUI on macOS, jamovi
28.17.0.0. The fixture generator creates data only; it does not
construct analysis options or results. Open an `.omv` directly with
jamovi. After rebuilding and installing this source, the examples also
appear under Open → Data Library → jsurvival, using the titles in
`jamovi/0000.yaml`.

The cohort examples contain 150 independent simulated patients, 106
observed events and 44 right-censored observations. `time` and
`followup` are months. `event` uses 1 for the event and 0 for censoring.
`Control` and `Treatment` are illustrative group labels; the data do not
establish a treatment effect.

| File in `data/` | Saved analysis and selected arguments |
|----|----|
| [singlearm_example.omv](https://www.serdarbalci.com/jsurvival/data/singlearm_example.omv) | Single Arm Survival: Kaplan-Meier, confidence intervals, risk table, censor marks, person-time rates and summaries. |
| [survival_example.omv](https://www.serdarbalci.com/jsurvival/data/survival_example.omv) | Survival Analysis: group comparison, age adjustment using `age`, adjusted curves, PH diagnostics, KM with confidence intervals/risk table/censor marks, and RMST. `rmst_tau = 0` selects the automatic horizon. |
| [survivalcont_example.omv](https://www.serdarbalci.com/jsurvival/data/survivalcont_example.omv) | Continuous-marker survival: `marker`, exploratory optimal cut-off, KM groups and exported cut-off group. A selected cut-off requires independent validation. |
| [multisurvival_example.omv](https://www.serdarbalci.com/jsurvival/data/multisurvival_example.omv) | Multivariable Cox: categorical `group` and `stage`, continuous `age` and `marker`, finalfit forest and PH diagnostics. No interaction is fitted. |
| [lassocox_example.omv](https://www.serdarbalci.com/jsurvival/data/lassocox_example.omv) | LASSO Cox: the same four predictors, explicit event/censor levels, five CV folds, `lambda.min`, seed 123456 and internal standardization; CV, coefficient and apparent risk-group plots. |
| [oddsratio_example.omv](https://www.serdarbalci.com/jsurvival/data/oddsratio_example.omv) | Firth logistic regression of `event` on `group`, `age` and `marker`, with explanations and the nomogram safeguard requested. See the expected warnings below. |
| [datetimeconverter_example.omv](https://www.serdarbalci.com/jsurvival/data/datetimeconverter_example.omv) | Eight rows; `iso_date`, YMD, UTC, preview 10, year/month/day extraction and exported year. Also contains `mixed_date` with malformed text and numeric `excel_date` for further exploration. |
| [outcomeorganizer_example.omv](https://www.serdarbalci.com/jsurvival/data/outcomeorganizer_example.omv) | Overall survival: explicit AWOD/AWD/DOD/DOOC mapping, follow-up and invented patient IDs, diagnostics, visualization and exported recoded outcome. Both death categories are events. |
| [timeinterval_example.omv](https://www.serdarbalci.com/jsurvival/data/timeinterval_example.omv) | ISO start/end dates, standardized months (`days / 30.4375`), quality metrics, summary and exported calculated time. |

## Options exercised in the GUI

- DateTime: automatic versus explicit YMD, system timezone versus UTC,
  component extraction and output. A preview of 5 correctly produced the
  documented 10–100 constraint error; setting 10 recovered the analysis
  before saving.
- Time intervals: days versus months, calendar versus standardized
  duration, quality metrics and exported time. The saved example
  explicitly uses standardized months. Date rounding means these
  intervals need not exactly equal the original continuous simulated
  survival times.
- Outcomes: overall survival and competing risks. The latter retained 82
  disease deaths, 24 other-cause deaths and 44 censored observations;
  the saved OS example has 106 events. No unknown status was coded as
  alive.
- Survival: plots, confidence/risk/censor displays, person-time, age
  adjustment, adjusted curves, PH diagnostics, RMST and exploratory
  cut-off output across the relevant examples. The saved RMST horizon is
  automatic, not a fixed 24 months.
- Multivariable: categorical and continuous predictors, forest and PH
  plots. The reopened interaction panel listed all four available
  predictors. An interaction model and the alternate forest style were
  not completed in this GUI pass.
- LASSO: 10, 6 and 5 folds, and the 1-SE and minimum-error rules. The
  final saved settings are five folds and minimum error. Figures were
  explicitly rendered, saved again and visually verified after
  reopening.
- Odds ratios: ordinary and Firth logistic fits and explanatory output.
  Requesting the nomogram with Firth correctly displays that it is
  unavailable rather than drawing a nomogram from a different fitted
  model.

The odds-ratio file deliberately keeps the Firth nomogram warning.
Diagnostic metrics also wait for an explicitly selected diagnostic
predictor and its positive and negative levels. The logistic model and
forest are complete. Its binary outcome records whether an event was
observed during varying follow-up; these odds ratios are not
fixed-horizon survival risks, diagnostic accuracy estimates or risk
ratios.

## Verification and remaining limitation

`Rscript --vanilla data-raw/validate-gui-examples.R` passes 90 checks:
nine matching analysis identities, completed status and no
result-element errors; exact catalog metadata; row/event counts; cached
plot resources; exported year and interval arithmetic; overall-survival
mapping; an independent
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
comparison of the displayed multivariable HRs; LASSO coefficient/HR
arithmetic and saved tuning; and the Firth safeguard. All nine ZIP
integrity checks passed; file hashes are recorded in
`data-raw/gui-examples-sha256.txt`. Compiler `--prepare` also passed in
an isolated copy, copied the examples and preserved all ten dataset
entries (including the existing histopathology example). The compiler
was given the observed app version explicitly because its automatic
app-version probe failed. Completed status alone did not establish that
plots had been cached: the initial LASSO save lacked images.
Recalculation, figure rendering and a further GUI save resolved that
file; the final archive contains all three requested plot images.

DateTime’s missing component row still displays `NaN` in some native
numeric preview cells. Inspection finds literal `"NA"` strings in the
serialized year/month component columns, while the day column uses a
proper missing cell. Parsing counts are correct (8 total, 1 originally
missing, 7 parsed, 0 failed; percentages 100%, 12.5%, 87.5%, 0%), and
the exported year contains a genuine missing value. This preview
presentation issue remains open and is distinct from the repaired
parsing-count denominator issue.

These bounded GUI and arithmetic checks do not complete every option
combination, theme, export format, interaction model or final release
acceptance check. The installed candidate used for creation has SHA-256
`81774a0042a441426473dc903964418fdb72993f95557343b7296a752bb95646`; the
source candidate was subsequently rebuilt for catalog wording as
recorded in `RELEASE_CHECKLIST.md`. Neither existing archive includes
these newly added data files: rebuild before library submission. No
module was submitted or published.

## Maintaining the files

- `data-raw/gui-examples.R` reproduces the initial synthetic data only,
  with seed
  11082026. It refuses to overwrite saved examples. Recreate data in a
            clean copy if needed, then add/configure analyses through
            the GUI again.
- `data-raw/gui-examples.yaml` is the maintained metadata for the new
  examples. `Rscript --vanilla data-raw/register-gui-examples.R` uses
  the umbrella’s `document_module_omv()` helper to append missing
  entries and verify them. `prepare()` preserves the datasets section;
  no unsupported configuration key is needed. Analysis registry entries
  remain generated.
- The validator reads the integer nominal output in the outcome example
  directly from the archive because `jmvReadWrite::read_omv()` currently
  rejects that Integer/Output combination. This is a reader limitation,
  not a jamovi file error; the archive is not modified by validation.
