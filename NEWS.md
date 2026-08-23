# jsurvival 1.0.6.03 (2026-08-22)

This release corrects interpretation text that was statistically wrong, removes clinical advice from
results, and stops third-party package notices appearing in the results pane. One defect produced a
garbled and incorrect sentence in the most-used analysis and is listed first.

## Behaviour changes -- saved analyses may produce different results

- **`survival`: the Cox summary sentence no longer states a hazard ratio as a risk ratio, and no
  longer prints a whole formatted cell where a number belongs.**The natural-language Cox summary
  interpolated finalfit's formatted output -- a string such as `1.50 (1.10-2.05, p=0.010)` -- into a
  sentence expecting a single value, producing *"there is 1.50 (1.10-2.05, p=0.010) times risk than
  when Grade is 1"*. Beyond being unreadable, "times risk" is wrong: a hazard ratio is a ratio of
  instantaneous event rates among those still at risk, not a ratio of cumulative risks. The sentence
  now reads as an estimated hazard ratio relative to the reference group, states what a hazard ratio
  is, notes the proportional-hazards assumption it carries, and explains that a confidence interval
  including 1 means the data are compatible with no difference in hazard.

## Fixed -- statements that were not true

- **`survival`: a non-significant interaction test no longer establishes consistency.**Failing to
  reach p < 0.05 in an interaction test was reported as the effect being consistent across
  subgroups. Interaction tests in survival data are markedly underpowered, so this is absence of
  evidence rather than evidence of absence; the text now says so.

- **`survival`: the p-value explanation is correct.**The panel described p < 0.05 in terms that
  implied the probability the finding is due to chance. It now states what the p-value is computed
  under.

- **`multisurvival`: the landmark caveat is present.**With landmark analysis enabled, every patient
  whose follow-up ended before the landmark is dropped and the clock is restarted at the landmark.
  Neither was disclosed, so a cohort could shrink and a time origin move without explanation. Both
  are now stated where the results are shown.

## Changed -- results no longer give clinical advice

- Text that told the reader what to do with a patient, or ruled on whether a model was fit for
  clinical use, has been replaced by explanation of what was estimated and how to read it. Guidance
  about running the analysis itself -- cohort size, assumption checks, which comparison is being
  made -- is unchanged and remains.

## Fixed -- output hygiene

- **R package chatter no longer appears in Analysis Notes.**jamovi surfaces `message()` and
  `warning()` conditions to the user, so notices from survival plotting and modelling packages
  leaked into results. Third-party calls in `survival`, `multisurvival`, `survivalcont`, `singlearm`
  and `oddsratio` are now wrapped so package chatter and deprecation notices are suppressed, while
  substantive warnings such as non-convergence still reach you.

- **A malformed reference no longer prevents results from rendering.**A citation with an empty
  publication year caused a serialization failure that produced no output at all, with an error
  mentioning `serialize` rather than anything about the analysis. All references now carry a year.

# jsurvival 1.0.4 (2026-08-07)

No analysis changes; the documentation was overhauled. This release keeps the module version in
step with the ClinicoPath suite, which moved to 1.0.4 across all of its modules.

Every survival analysis this module ships -- `datetimeconverter`, `multisurvival`, `oddsratio`,
`outcomeorganizer`, `singlearm`, `survival`, `survivalcont` and `timeinterval` -- is unchanged since
1.0.2. The diff against that release touches only version strings, the `date:` field, and two
ignore files (`.gitignore`, `.Rbuildignore`, for local scratch directories); filtering those out of
`R/` and `jamovi/` leaves no lines of behavioural change. The release-review work done elsewhere in
the suite during this window landed in `meddecide` and `OncoPath` and is documented in those
modules' NEWS files.

`DESCRIPTION` (Version 1.0.4, Date 2026-08-06) and `jamovi/0000.yaml` (version 1.0.4) agree, which
is a prerequisite for the release workflow added in 1.0.2 -- it refuses to tag when the two disagree.

## Documentation

All 53 files under `vignettes/` were audited against `jamovi/0000.yaml` and the generated wrapper
signatures. These articles are published to <https://www.serdarbalci.com/jsurvival/articles/>;
`vignettes/` is excluded by `.Rbuildignore` and there is no `VignetteBuilder`, so none of this
affects `R CMD check`.

- **Option coverage went from 78% to 95%.**Of the 304 options across the eight shipped analyses,
  67 were not mentioned anywhere in the documentation; 16 remain. Every example added below was
  executed against the bundled `histopathology` data before being written down.
- **`survival()` had 28 undocumented options, several of them substantial features added between
  February and July 2026.**`08-advanced-topics.Rmd` now covers them: weighted log-rank tests
  (`weightedLogRank`, `survivalTestType`, with the Fleming-Harrington family reported alongside the
  standard test); the seven age-correction options (`age_adjustment`, `age_variable`,
  `age_interaction`, `age_stratified_cox`, `age_group_cutpoints`, `age_time_scale`,
  `age_standardization` with `age_standardization_method`, `age_stratified_km`); parametric
  survival models across eight distributions including flexible splines; calibration curves;
  restricted cubic splines for non-linearity; bootstrap internal validation; adjusted survival
  curves; and the REMARK reporting checklist.
- **`datetimeconverter` was shipped with no documentation at all** - 30 of its 32 options went
  unmentioned. `11-data-preparation.Rmd` now covers it as the step before interval calculation:
  the nine input formats, why naming the format beats `auto` when a dataset mixes `dmy` and `mdy`,
  the eleven component extractors, and the quality report that names rows which failed to parse
  rather than letting them become silent `NA`s. The worked example reports 5 of 6 rows parsed
  (83.33%) with the failure listed by row number.
- **`multisurvival`'s model-performance options are documented**: optimism-corrected C-index by
  bootstrap (`ci_optimism`, `ci_optimism_boot`), covariate contribution by single-term deletion
  (`compare_models`), Brier score and time-dependent AUC (`show_survmetrics`,
  `survmetrics_timepoints`), and adjusted probability summaries (`ac_summary`).
- **A whole article documented a feature that does not exist.**
  `01-multisurvival-time-dependent-comprehensive.Rmd` describes time-dependent covariates in
  `multisurvival`, using 17 options that are commented out in `jamovi/multisurvival.a.yaml` under
  headings reading *"EXPERIMENTAL - will be implemented later"* - the time-dependent, frailty and
  spline groups. Sixty options are commented out there in total. The article is kept as the design
  specification for that future release and now opens by saying the code is not yet runnable; its
  chunks were already `eval = FALSE`. `multisurvival_documentation.md` tabulates 19 such options
  and now names them.
- **`survivalcont` was shown with a `padjustmethod` argument.**No such option exists, or ever has.
  The passage now says so and documents the options that do control multiple cut-points
  (`multiple_cutoffs`, `num_cutoffs`, `cutoff_method`). The same article claimed restricted cubic
  splines "would require additional implementation beyond basic jSurvival"; `survival()` has had
  `rcs_analysis` for some time, and the passage now shows it.
- **Twenty-three articles document analyses jsurvival does not ship, and now say so.**
  `alluvialSurvival`, `comparingSurvival`, `competingsurvival`, `coxdiagnostics`, `datecorrection`,
  `groupedforest`, `jvisr`, `oneSurvival`, `powersurvival`, `simonmakuch`, `stagemigration`,
  `subgroupforest`, `survivalPower`, `timeroc`, `lassocox` and `jiwillsurvive` are all on
  development or test menu routes in the umbrella ClinicoPath module and reach no user today.
  Separately, `03-treatment-response.Rmd` uses `crosstable` and `summarydata` (which ship in
  **ClinicoPathDescriptives**) and `waterfall` (**OncoPath**), and now names the module to install
  for those steps. Nothing was deleted.
- All 48 calls to jsurvival's own analyses across the 53 files were checked against the generated
  wrapper signatures; the eight failures were the `padjustmethod` call and the seven
  time-dependent-covariate calls described above.

# jsurvival 1.0.3 (2026-08-04)

An intermediate suite-wide version bump with no jsurvival content. No analysis, option, output or
test in this module changed; the commit propagated the 1.0.3 version string and the package date.

# jsurvival 1.0.2 (2026-08-03)

All eight analyses were reviewed for this release. Six had a final pre-release pass checking every
reported quantity against an independent reference (`survival`, `survRM2`, `riskRegression`,
`epiR`, `logistf`, `lubridate`); `singlearm` and `survivalcont` were audited earlier in the cycle,
and the adjusted-estimation work below spans `multisurvival` and `singlearm` together.

The estimators were already correct -- Cox hazard ratios, Kaplan-Meier medians, RMST, C-index,
Brier scores and adjusted survival curves all reproduce their references exactly. What needed
fixing was the boundary between *estimated* and *not estimable*, cases where two parts of one
report answered the same question differently, and several statements the output made about
itself.

## Fixed

### One estimator behind every panel

- **Changing the adjustment method moved the plot but not the tables.**`ac_method` was read in
  exactly one place -- the call to `survminer::ggadjustedcurves()` inside the plot. The adjusted
  survival table, the adjusted median table and the accompanying narrative each built their own
  prediction instead, from a single mean/mode covariate profile. So `average` and `conditional`
  produced **byte-identical tables** while the plot changed, and nothing said so; `survminer`
  documents those as distinct estimands, not display variants. Plot and table could therefore
  disagree inside one report. All consumers now read a single shared estimator,
  `.adjustedCurveData()`. (`multisurvival`, `singlearm`)
- **"Observed at risk" and "observed events" in the adjusted tables were whole-cohort numbers
  wearing a group label.**They came from the model's common risk set, so they were identical for
  every level of the adjustment variable while the narrative described them as group-specific.
  The columns are now titled for what they are, and the estimand is stated above the table.
- **"Adjusted Cox Model Results" was a different model from the main table.**In one real run the
  main multivariable table showed `performance_status` as a factor with two rows and 13
  likelihood-ratio degrees of freedom, while the adjusted table showed it as one continuous row
  with 12 df -- two models fitted, both displayed, nothing saying so. One code path honoured the
  column's factor type and the other coerced it to a score. Both now derive from the same fitted
  object. (`multisurvival`)
- **Competing-risk detection ignored a recoded outcome column.**The guard read
  `multievent && analysistype == "compete"`, which is blind to the `outcomeorganizer` hand-off -- a
  recoded `Censored`/`Event`/`Competing` column arrives already coded 0/1/2 with `multievent`
  left off, which is the entire point of that column. The 0/1/2 vector then flowed into code that
  assumes a 0/1 indicator. Detection is now driven by the status vector itself.
  (`survivalcont`, and the same fix across the family)

### `singlearm` and `survivalcont`

- **`singlearm` results did not invalidate when the outcome recoding, landmark or time units
  changed.**`analysistype`, the four event-level mappings, `uselandmark`, `landmark`,
  `timetypeoutput` and `timetypedata` were missing from the `clearWith` of the median table,
  survival table, person-time table, baseline-hazard table and every plot.
- **`singlearm` multi-event level validation missed unset levels.**The check collected the four
  level options with `c()`, which silently drops `NULL`s, so a missing assignment went undetected
  instead of being reported. Each level is now checked individually.
- **`singlearm` named the wrong confidence-interval method for the median.**The note said
  `conf.type = 'log-log'`; `survfit()`'s default is `'log'` -- Greenwood's variance on the log
  scale. No interval changed, only the label.
- **`singlearm` described its survival table's event rate inaccurately.**"Event rate by this
  timepoint" is events *within the interval* expressed as a proportion of the initial cohort, and
  now says so.
- **`singlearm` cutpoints are now parsed and support-checked.**`.resolveCutpoints()` handles
  malformed input and `.supportedCutpoints()` refuses time points the data cannot support --
  including a zero-width person-time boundary, which yields no useful rate. Y-axis limits outside
  0-1, or inverted, now produce an actionable message instead of a broken plot.
- **`survivalcont` stranded its outputs when cut-point search ran.**`.run()` was restructured so
  results are populated regardless of the `findcut` path, with `deleteRows()` guards against
  double-population, notice reset between runs, an explicit error below ten events, and a
  validation halt that stops rather than continuing on invalid input.

### Estimated vs. not estimable, and output that overstated itself

- **`survival` reported survival probabilities beyond the observed follow-up.**The 1/3/5-year
  table is built with `summary(fit, times =, extend = TRUE)`, which carries the last Kaplan-Meier
  estimate forward indefinitely. With the default cutpoints (12, 36, 60) and a cohort followed for
  two years it printed a "60-month survival" -- with a confidence interval -- computed from **zero
  patients at risk**: 0.0% in one group and 4.5% (0.8-25.7%) in the other. Cutpoints past a
  group's last observation are now omitted, with a note naming them and each group's longest
  follow-up. This is the same rule `.calculateRMST()` already applied; it is now applied here too.
- **`survival` aborted entirely when a cutpoint was mistyped.**`as.numeric("abc")` is `NA`, and
  `summary.survfit()` then stops with `times contains missing values` -- taking the median table,
  the Cox output and every plot down with it, for one typo in a free-text box. Non-numeric and
  negative cutpoints are now ignored with a note, duplicates are dropped, and an empty box falls
  back to the documented default.
- **`survival` tables did not always refresh when the option driving them changed.**`cutp` was
  missing from the 1/3/5-year table's `clearWith`, and `analysistype`, the four event-level
  mappings and the landmark options were missing from four tables. `jmvcore`'s `Table$addRow()`
  appends with no duplicate-key check, so an uncleared table keeps the old rows alongside the new.
- **`survival` told users to judge significance by eye from two confidence intervals.**The
  interpretation panel stated "Non-overlapping confidence intervals suggest significant
  differences" as a decision rule, alongside "Use for patient stratification and treatment
  decisions". Overlapping intervals do not imply the absence of a difference; the text now points
  to the log-rank test and drops the treatment-decision claim.
- **`multisurvival` reported no joint test for a multi-degree-of-freedom interaction.**The
  interaction table gives one row per coefficient, each a 1-df Wald test. For a 3-level x 2-level
  interaction that is two rows -- p = 0.077 and p = 0.726 -- and neither answers "is the effect
  modified by this variable at all?" The joint 2-df test over the same model is p = 0.154. It is
  now reported alongside the rows (Wald, agreeing with the likelihood-ratio test to three
  decimals: chi^2 = 3.7426 / 2 df / p = 0.1539 against 3.7466 / 2 / 0.1536).
- **`multisurvival` person-time could show figures from a previous covariate set.**Person-time is
  computed on the complete-case set across *all* selected variables, so adding one continuous
  covariate with missing values moved the overall row from 134 events / 8235.5 person-time to
  94 / 6005.47 -- but `contexpl` was absent from that table's `clearWith` and the table never
  cleared its rows. Both are fixed.
- **`oddsratio` printed a non-estimable odds ratio as a precise number.**Under (quasi-)separation
  `glm`'s IRLS halts wherever the iteration limit leaves it, and the table rendered
  `118848049086800030859264.00 (0.00-Inf, p=1.000)` -- an odds ratio of 1.19 x 10^2^3 shown to two
  decimal places. The cell now reads `not estimable`, with a strong warning naming the variable
  and pointing to Firth penalized regression. The rule keys on the confidence interval being
  unbounded, so a large but genuinely estimable odds ratio is untouched.
- **`oddsratio` reported diagnostic metrics with no uncertainty.**Sensitivity, specificity and
  the likelihood ratios were bare point estimates: 63.8% from 20 patients and from 2000 read
  identically. They now carry Clopper-Pearson exact intervals for the proportions and the log
  method (Simel et al. 1991) for the likelihood ratios -- reproducing `epiR::epi.tests()`
  bit-identically (maximum absolute difference 0 across 4,156 two-by-two tables). The arithmetic
  is adapted from epiR (GPL >= 2, credited in the source) rather than imported, so `jsurvival` does
  not gain an epidemiology package as a dependency for one call.
- **`oddsratio` named the wrong confidence-interval method.**The footnote said "Wilson score";
  `epi.tests()` defaults to `method = "exact"`, which is Clopper-Pearson. Wilson differs in the
  third decimal.
- **`datetimeconverter` warned "Implausible Dates Detected" on every successful conversion.**A
  bare `format()` inside the package resolves to `jmvcore::format`, which ignores the `"%Y"`
  format string; `as.integer()` of the whole datetime string then yielded the epoch seconds
  (1710498030 for 2024-03-15) against an upper bound that had become 21033, so every ordinary date
  compared as out of range. A warning that fires on 100%-successful conversions is worse than
  none -- it trains users to ignore the one signal that a wrong format has mis-parsed their dates.
  The check now discriminates correctly: silent on 2022-2024, firing on 1850 and 2100.
- **`datetimeconverter` showed large numbers in scientific notation -- and parsed them that way.**
  The same masking made `scientific = FALSE` a no-op, so a Unix epoch appeared in the preview's
  "Original Value" column as `1.7e+09`. In the numeric fallback branch that string is also what
  gets handed to the date parser, turning a valid number into an unparseable date.
- **`datetimeconverter` documentation contradicted the tool.**The glossary gave Excel serial
  45000 as "May 18, 2023"; it is **15 March 2023**, which is what the converter itself returns.
  The Excel entry now also explains the 1899-12-30 origin and the 1900 leap-year quirk.
- **`datetimeconverter` claimed its numeric output was timezone-independent.**A date with no time
  is midnight *in the selected zone*, so `2024-01-15` becomes 1705276800 under UTC and 1705266000
  under Europe/Istanbul. With the default ("system") two collaborators on different machines get
  different numbers from the same file. The option text now says so and tells you to select UTC
  when the value must be reproducible.
- **`timeinterval` could write the calculated column back with no row mapping.**`self$data` holds
  only the columns the analysis asked for, so it has one column whenever the same variable is
  chosen as both start and end date; subsetting without `drop = FALSE` collapsed it to a vector,
  `rownames()` returned `NULL`, and `setRowNums(NULL)` then misaligned the written column.
- **`timeinterval` extreme-value filtering deleted genuine follow-up.**The rule is
  "> multiplier x 99th percentile", which only orders correctly for a positive 99th percentile. In
  a cohort where 99% of patients enter and exit on the same day the threshold is 0, so every
  non-zero interval counted as extreme and the handful of real follow-ups were dropped. The filter
  is now skipped in that case, with the reason stated under "Filters applied".
- **`outcomeorganizer` lost the competing-risk flag when no competing event was observed.**The
  recoded column was exported as a character vector, and `jmvcore` derives a column's levels from
  the values present -- so a competing-risks run on a cohort without other-cause deaths shipped a
  column declaring only `Censored`/`Event`. Downstream, `survival` and `multisurvival` identify
  this interchange format by requiring all three declared levels, so the hand-off was not
  recognised: the analysis stopped with "Event Level is not selected", or ran as plain
  Kaplan-Meier if the user picked "Event" to get past it. The column is now exported as a factor
  declaring `Censored`/`Event`/`Competing` whether or not all three occur, which also puts the
  levels in the order the 0/1/2 codes imply rather than alphabetically.

## Changed

- **Numerical verification tests now verify numbers.**The six `test-*-verification.R` files
  computed reference fits -- and in one case wrote the right answer in a comment -- then asserted
  only that an object came back or that a table had rows. They now compare the rendered values
  against `survival`, `survRM2`, `riskRegression`, `epiR`, `logistf`, `stats::t.test`,
  `poisson.test` and `lubridate`, and pin every fix above with a regression test.
- **`survival`: four options that did nothing were removed.**`hazard_plots`,
  `parametric_extrapolation`, `extrapolation_time` and `parametric_diagnostics` had stub backends
  and permanently hidden results, but the jamovi UI compiler adds a control for every declared
  option -- so they appeared as live checkboxes, one of them ticked by default. Saved `.omv` files
  referencing them still load.

## Added

- **Automated GitHub release (`.github/workflows/release.yaml`).**A push to the default branch
  touching `DESCRIPTION` or `jamovi/0000.yaml` cross-checks the two version strings, refuses to
  proceed if they disagree, and -- if the tag does not already exist -- tags `v<version>` and
  publishes a release whose notes are the matching section of this file.

# jsurvival 1.0.0 (2026-07-13)

## First stable release

- Fixed both odds-ratio validation paths so empty or invalid data produce clear
  analysis-level errors instead of an R6 method failure.
- Replaced non-structural named HTML entities with Unicode and converted fragmented
  translated output to complete placeholder-based sentences.
- Normalized checkbox labels to noun phrases, removed stale `sas` invalidation keys,
  and hid unfinished survival-tree controls until the feature is wired end to end.
- Trimmed `00refs.yaml` to the 22 citations used by the eight analyses and replaced
  broad package imports with selective imports.
- Synchronized `DESCRIPTION`, jamovi metadata, and `CITATION.cff`; removed the orphaned
  survival-power event handler.

# jsurvival 0.0.47 (2026-07-05)

## Bug Fixes

* **Fixed a crash in competing-risks analysis on jamovi installs.**`cmprsk::cuminc()` is called by the survival, single-arm, continuous-survival, and multivariable-survival analyses, but `cmprsk` was missing from the package `Imports`. Because jamovi installs only a package's `Imports`, competing-risks analysis failed on a clean install with "there is no package called 'cmprsk'". `cmprsk` is now declared.
* Declared `digest`, `pec`, and `rpart.plot` (previously used via `::` but undeclared). Moved the optional machine-learning survival backends (`randomForestSRC`, `xgboost`, `survivalsvm`, `SurvMetrics`) to `Suggests` so they degrade gracefully when not installed.

# jsurvival 0.0.46 (2026-07-04)

*This release consolidates versions 0.0.32.62 through 0.0.46 into a single entry. Headline themes: a major expansion of the univariate **Survival** module toward REMARK-compliant prognostic reporting (age-adjusted analysis, a weighted log-rank test family, calibration assessment, non-linearity testing, and bootstrap internal validation); Cox interaction / effect-modification terms in **Multivariable Survival**; Firth penalized logistic regression in **Odds Ratio**; and a unified multi-tier HTML notice system across modules. Minimum jamovi version raised to 2.7.27.*

## New Statistical Features

### Univariate Survival (`survival`)

- **Age-adjusted survival analysis** -- new option group controlling age adjustment (`age_adjustment`, `age_variable`):
  - `age_interaction` -- test an age x group interaction (new `ageInteractionTable` with coefficient, HR, SE, z, p-value).
  - `age_stratified_cox` and `ageAdjustedCoxTable` comparing unadjusted vs. age-adjusted hazard ratios side by side.
  - `age_time_scale` -- fit a Cox model using age as the time scale (new `ageTimeScaleTable`).
  - `age_standardization` with `age_standardization_method` (`indirect`/SMR or `direct`) producing an `ageStandardizationTable` (observed/expected deaths, SMR with 95% CI).
  - `age_stratified_km` and `age_group_cutpoints` for age-stratified Kaplan-Meier curves (`ageStratifiedKMPlot`).
  - `adjusted_curves` -- covariate-adjusted survival curves (`adjustedCurvesPlot`).
  - Interpretation panels: `ageAdjustedInterpretation`, `ageAdjustedExplanation`, `ageTimeScaleInterpretation`, `ageStandardizationInterpretation`.
- **Weighted log-rank test family** -- new `weightedLogRank` option with `survivalTestType` choices: `logrank`, `gehan_breslow`, `tarone_ware`, `peto_peto`, and `fleming_harrington`; results in `weightedLogRankTable` (test, rho, chi-square, df, p-value, weighting) plus a `weightedLogRankExplanation` panel. Pairwise group comparisons now honor the selected rho weighting.
- **Calibration assessment** -- `calibration_curves` with `calibration_timepoint` and `calibration_ngroups`; outputs `calibrationTable`, per-group `calibrationGroupTable` (predicted vs. observed with CIs), `calibrationPlot`, and `calibrationInterpretation`.
- **Non-linearity assessment (restricted cubic splines)**-- `rcs_analysis` with `rcs_variable` and `rcs_knots`; outputs `rcsTestTable` (model, df, log-likelihood, AIC, LR chi-square, p-value, conclusion), `rcsPlot`, and `rcsInterpretation`.
- **Bootstrap internal validation** -- `bootstrapValidation` with `bootstrapValN` resamples; `bootstrapValidationTable` reports apparent, optimism, and optimism-corrected metrics with a `bootstrapValidationExplanation`.
- **REMARK reporting checklist** -- `remark_checklist` option renders a `remarkChecklist` HTML panel for prognostic-marker reporting.
- **Parametric survival scaffolding** -- UI options added (`use_parametric`, `parametric_distribution` covering exponential, Weibull, log-normal, log-logistic, gamma, generalized gamma, Gompertz, and Royston-Parmar spline; `spline_knots`, `spline_scale`, `parametric_covariates`, `parametric_extrapolation`, `extrapolation_time`, `parametric_diagnostics`, `compare_distributions`, `parametric_survival_plots`, `hazard_plots`). The parametric backend remains disabled/experimental in this release.

### Multivariable Survival (`multisurvival`)

- **Cox interaction / effect-modification terms** -- new `interactions` option (type `Terms`) that crosses variables already chosen as explanatory or continuous-explanatory predictors.
  - New `interactionTest` table (interaction HR with 95% CI and p-value) and `subgroupHR` table (within-subgroup hazard ratios by moderator level).
  - New pure, unit-testable helper module `R/multisurvival-interactions.R` (term mapping, formula construction, moderator/subgroup summaries), separated from the R6 backend for maintainability.
  - New `jamovi/js/multisurvival.events.js` model-builder events that populate the interaction predictor pool from the explanatory/contexpl boxes and prune stale terms.

### Odds Ratio (`oddsratio`)

- **Firth penalized logistic regression** -- new `usePenalized` option (via `logistf`) to reduce small-sample bias and handle separation, with profile-likelihood confidence intervals and automatic fallback to standard logistic regression when `logistf` is unavailable.
- New `predictorLevel` option to set the positive level of the predictor.

## Enhanced Existing Modules

### Multivariable Survival (`multisurvival`)

- Proportional-hazards testing (`ph_cox`) now defaults to **on**, surfacing global and per-covariate Schoenfeld residual statistics via `survival::cox.zph` (aligned with REMARK reporting guidance).

### DateTime Converter (`datetimeconverter`)

- Added a unified HTML `notices` panel for validation and conversion messaging.

## Notices & Messaging System

- Introduced a four-tier structured HTML notice system (`errors`, `strongWarnings`, `warnings`, `infoMessages`) with `.addHtmlMessage()` / `.initializeMessageOutputs()` helpers, wired into `survival`, `multisurvival`, `oddsratio`, and `outcomeorganizer`. Message outputs are reset at the start of each run to prevent accumulation across runs.

## Module Removals

- Removed the **Date/DateTime Validator** (`datevalidator`) module (backend, header, and all `.a/.r/.u.yaml` files deleted).

## Package Infrastructure

- Raised the minimum jamovi application version (`minApp`) from 1.8.1 to **2.7.27**.
- Added internal helper library `R/diagnostichelpers.R` with diagnostic-accuracy functions (sensitivity, specificity, PPV/NPV with optional prevalence/Bayes adjustment, positive/negative likelihood ratios, diagnostic odds ratio, Youden's J).
- Added internal helper library `R/survivalPower_distributions.R` implementing Weibull, log-normal, and piecewise-exponential parameterizations and expected-events calculations (Lachin & Foulkes reference).
- Expanded dataset roxygen documentation in `R/data.R` (histopathology, melanoma, longitudinal, and stage-migration test datasets).
- Reworked reproducible-syntax generation (`asSource`) and hardened R-string escaping across module functions. `R/utils.R` gains new formula helpers (`.asSurvivalFormula()`, `.escapeVariableNames()`, `.buildSurvivalFormula()`) and `%notin%`/`%!in%` operators so that variable names containing special characters produce valid Syntax-mode output.
- Hardened error handling in **Time Interval Calculator** (`timeinterval`) with additional `jmvcore::reject()` guards for invalid or missing dates and unsupported date formats.
- Updated bibliographic references in `jamovi/00refs.yaml`.

---

# jsurvival 0.0.32.60 (2025-12-28)

## Major Updates

### New Modules
- **Date Validator**: Comprehensive date and datetime validation tool.
  - Validates and diagnoses messy date/datetime formats using multiple methods (datefixR, anytime, lubridate).
  - Provides detailed audit tables and quality assessment reports.

### Re-introduced and Enhanced Modules
- **Outcome Organizer**: Re-introduced with significant enhancements.
  - Improved data cleaning and label handling.
  - Contextual validation for different survival analysis types (OS, Cause-specific, etc.).
  - Diagnostic messages and glossary for educational support.
- **Time Interval**: Re-introduced as a comprehensive calculator.
  - Robust time interval calculations with landmark analysis support.
  - Person-time calculation for epidemiological studies.
  - Data quality assessment for time intervals.

## Enhancements

### Survival Analysis
- **Parametric Models**: Improved handling of covariates and formula construction.
- **Plots**: Added logic to skip certain plots (e.g., PH assumption, residuals) for competing risk analysis to avoid errors.
- **Diagnostics**: Enhanced model summaries and p-value calculations.

### General
- **Notice System**: Improved notice generation and handling across modules.
- **Documentation**: Updated documentation for all modules.

---

# jsurvival 0.0.31.84 (2025-10-03)

## New Features

### Educational Explanations
- Added `showExplanations` option to survival, survivalcont, multisurvival, and oddsratio modules
- Built-in HTML explanations for key analysis concepts and statistical methods
- Context-sensitive educational content to help users understand their analyses

### Single Arm Survival Enhancements
- **Clinical Analysis Presets**: Pre-configured settings for common study types
  - Overall survival analysis (most common)
  - Disease-free survival analysis
  - Treatment effectiveness study
  - Post-surgical outcomes
  - Custom analysis (advanced users)
- **Guided Setup Mode**: Step-by-step guidance for users new to survival analysis
- Enhanced person-time calculations and natural language summaries

### Stage Migration Analysis
- Advanced TNM staging validation and trend analysis
- Improved robustness and explanations for stagemigration module
- Multifactorial analysis capabilities
- Granular table controls and debug outputs

### Survival Analysis Module
- Enhanced educational explanations for univariate survival
- Improved Cox regression output and interpretation
- Additional visualization options

### Continuous Survival Analysis
- Educational explanations for cut-point analysis
- Enhanced optimal threshold determination methods
- Improved output tables and visualizations

### Multivariable Survival
- Educational explanations for multivariable models
- Enhanced model diagnostics and output

### Odds Ratio Analysis
- Educational explanations for odds ratio interpretation
- Improved forest plots and summary tables

## Module Removals

- Removed `outcomeorganizer` module (functionality integrated into other modules)
- Removed `timeinterval` module (functionality integrated into other modules)

## Bug Fixes and Improvements

- Enhanced diagnostic test utility functions
- Improved tree analysis for survival models
- Better handling of missing data
- Updated documentation across all modules

## Documentation

- Updated package documentation and vignettes
- Enhanced inline help and tooltips
- Improved example datasets and use cases

---

# jsurvival 0.0.3.90 (2024-07-31)

## Major Changes

- Initial implementation of educational explanations system
- Refactored analysis modules for better user experience
- Enhanced natural language summary generation

## New Features

- Stage migration analysis for cancer staging studies
- Advanced survival model options
- Diagnostic test utility functions

---

# jsurvival 0.0.3.0 and earlier

For changes in earlier versions, please refer to the git commit history.
