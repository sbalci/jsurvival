# Saved jamovi examples

Ten synthetic workbooks contain thirteen saved analyses covering all nine
jsurvival functions. Open a file below in jamovi, read its introductory teaching
note, then select an analysis in the results pane to inspect its variables and
settings. The native notes explain the scenario, coding, saved setup, results
and suggested next steps. Save experiments under a new filename.

The analyses were configured and saved through the jamovi GUI on macOS using
jamovi 28.17.0.0. They can be opened directly. After rebuilding and installing
this source, they are also registered under Open → Data Library → jsurvival.
The examples are synthetic teaching material, not clinical evidence.

## Workbooks and saved features

The survival cohorts contain 150 independent simulated patients: 106 observed
events and 44 right-censored observations. `time` and `followup` are months;
`event = 1` is the observed endpoint and `event = 0` is censoring. `age` is in
years and `marker` is a standardized score in arbitrary units. Control and
Treatment are illustrative labels, not evidence of a treatment effect.

| Workbook in `data/` | Saved analysis and selected features |
| --- | --- |
| [singlearm_example.omv](data/singlearm_example.omv) | One whole-cohort Kaplan–Meier analysis with confidence intervals, risk table, censor marks, person-time rates and natural-language summaries. |
| [survival_example.omv](data/survival_example.omv) | One group comparison with age adjustment, adjusted curves, proportional-hazards diagnostics, Kaplan–Meier displays, summaries, explanations and REMARK guidance. RMST uses an explicit **24-month** horizon. |
| [survivalcont_example.omv](data/survivalcont_example.omv) | One continuous-marker Cox analysis with an exploratory optimal cut-off, grouped Kaplan–Meier curve, confidence intervals, risk table, exported cut-off group, explanations and summaries. The selected cut-off requires independent validation. |
| [multisurvival_example.omv](data/multisurvival_example.omv) | One Cox model with categorical `group` and `stage`, continuous `age` and `marker`, finalfit forest, proportional-hazards diagnostics and a summary. No interaction is fitted. |
| [lassocox_example.omv](data/lassocox_example.omv) | Two analyses compare `lambda.min` with `lambda.1se`, each using five CV folds, seed 123456 and standardization. Both contain CV, coefficient, coefficient-path and apparent risk-group plots, exported development risk scores, encoding/reproducibility details, reproducible R code and teaching explanations. |
| [oddsratio_example.omv](data/oddsratio_example.omv) | One Firth logistic model of `event` on `group`, `age` and `marker`, with explanations and the intentional nomogram safeguard described below. |
| [oddsratio_binary_example.omv](data/oddsratio_binary_example.omv) | One ordinary logistic model for 240 invented patients: nodal involvement at surgery is Positive versus Negative; regression predictors are `age`, `tumor_size` in millimetres and categorical `grade`. The separate `diagnostic_test` has explicit Positive/Negative coding for diagnostic metrics. Includes forest, prediction nomogram and explanations. |
| [datetimeconverter_example.omv](data/datetimeconverter_example.omv) | One analysis of eight ISO-date rows using YMD and UTC, year/month/day extraction, an exported year, quality assessment, summary, explanatory notes and glossary. Alternative columns demonstrate malformed text and Excel serial dates when selected. |
| [outcomeorganizer_example.omv](data/outcomeorganizer_example.omv) | Two analyses compare overall-survival and competing-risk recoding, each with an exported outcome, diagnostics, visualization, summary and glossary. AWOD/AWD are alive statuses; DOD/DOOC are disease/other-cause deaths. |
| [timeinterval_example.omv](data/timeinterval_example.omv) | Two analyses compare standardized with calendar-aware months, both with quality assessment, summary, glossary and exported calculated time. Standardized months are elapsed days / 30.4375; calendar months use whole monthly anniversaries plus the fraction of the actual span to the next anniversary. |

The outcome workbook retains 82 disease deaths, 24 other-cause deaths and 44
censored records. Its overall-survival export counts both death categories as
events. Its competing-risk export uses **Censored / Event / Competing** labels
to preserve the distinction when passed to survival analyses; the underlying
endpoint coding is 0 / 1 / 2, respectively. Recoding alone does not infer the
time to a composite endpoint's first event or fit a multistate model.

In the binary pathology workbook, the diagnostic table contains TP = 79,
FP = 20, FN = 12 and TN = 129. These unadjusted test summaries are separate
from the logistic model of age, tumour size and grade. Its prediction nomogram
shows the fitted model; it does not establish prediction accuracy in new data.
Likewise, LASSO risk-group separation and displayed development-sample
performance are apparent results, not external validation.

See the [feature map](data-raw/gui-example-feature-map.md) for additional settings
to explore beyond the saved starting points.

## Expected safeguards and presentation limits

The original odds-ratio workbook deliberately retains the Firth nomogram notice:
this implementation does not produce a Firth nomogram. Diagnostic metrics also
wait for a selected diagnostic predictor and explicit positive/negative levels.
Its model and forest are complete. Because its binary event outcome comes from
records with differing follow-up, these odds ratios are not fixed-horizon
survival risks, diagnostic accuracy estimates or risk ratios. Use the separate
binary pathology workbook for the completed diagnostic workflow.

DateTime's missing extracted components may still display `NaN` in some native
preview cells. The saved preview contains literal `"NA"` strings in its year/month
component columns, while the exported year has a genuine missing value. Parsing
counts are correct: 8 total, 1 originally missing, 7 parsed and 0 failed. This
preview presentation issue remains open.

Interactions, alternate forest styles, other endpoint types, themes and all
export formats are not exhaustively demonstrated or validated by this suite.

## Verification

On 2026-11-18, `Rscript --vanilla data-raw/validate-gui-examples.R` passed
**252 checks** across ten workbooks and thirteen completed module analyses.
It verifies identities, analysis/result errors, requested cached figures and
their resources, catalog metadata and exact native teaching notes. Independent
checks cover cohort counts, date conversion, both month definitions, both
outcome mappings, multivariable Cox HRs, binary logistic ORs and diagnostic
counts, LASSO settings/coefficient arithmetic and the Firth safeguard. The
archives contain 21 requested cached figures in total.

All ten annotated workbooks were reopened and saved in jamovi 28.17.0.0 on
2026-11-18. Their native teaching introductions were observed in the results pane;
the cached HTML exports also contain the notes. The final archive/numerical checks
were rerun after these saves. This does not establish every desktop interaction,
export format or final release acceptance.

## Local module bundle

The build-only candidate
[`jsurvival_1.1.4_examples_20261118.jmo`](jsurvival_1.1.4_examples_20261118.jmo)
contains these ten teaching workbooks and the existing histopathology workbook.
Install it in jamovi, then use Open → Data Library → jsurvival to select an example.
It was built in an isolated source copy without compiler warnings or errors;
30 bundle checks verified ZIP integrity, both metadata catalogs and all 22
workbook copies against the source bytes. The previous candidate was preserved.
This candidate was not installed or submitted during the example preparation.

SHA-256: `89ec47d8c08bf17110e2ad06bb5c510663ff7b457f65273208d003fa2efd466a`.
The [bundle verification report](data-raw/gui-examples-bundle-20261118.json)
records the command, source file digests and checks.

## Maintaining the files

- `data-raw/gui-examples.R` reproduces the original data only and refuses to
  overwrite saved workbooks. Configure analyses through jamovi and save them
  before validating; a completed status alone does not prove that requested
  images were cached. Render figures and save again when necessary.
- `data-raw/oddsratio-binary-example.R` generates the separate synthetic binary
  pathology dataset. Its logistic and diagnostic settings are saved through jamovi.
- `data-raw/gui-examples.yaml` maintains Data Library metadata.
  `Rscript --vanilla data-raw/register-gui-examples.R` synchronizes the entries
  without hand-editing the generated analysis registry. `prepare()` preserves
  the dataset section.
- `data-raw/gui-example-tutorials.json` maintains teaching introductions.
  `python3 data-raw/annotate-gui-examples.py` updates native jamovi notes while
  preserving data, model results and figures; `--check` verifies them without
  writing. Open and save in jamovi to refresh the cached HTML export as well.
- The validator reads nominal Integer/Output columns in the outcome workbook
  directly from the archive because `jmvReadWrite::read_omv()` currently rejects
  that combination. It verifies competing-risk labels separately from physical
  factor storage codes; validation does not modify the workbook.
- Refresh `data-raw/gui-examples-sha256.txt` after final saves, then rerun the
  validator and archive integrity checks against the final files.
