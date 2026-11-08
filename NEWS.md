# jsurvival 1.1.3 (2026-11-08)

- **GUI examples for all nine analyses:** Added synthetic `.omv` files in `data/`,
  each with a GUI-configured saved analysis, and registered descriptive names,
  coding, units and tags in the module's Data Library. Includes exported date,
  interval, outcome and cut-off columns; Kaplan-Meier and Cox displays; LASSO
  tuning; and a Firth nomogram safeguard example. See `GUI_EXAMPLES.md` for the
  exercised options, archive checks and the remaining DateTime missing-component
  display limitation. These examples do not establish clinical validity or full
  desktop release acceptance. Rebuild the module to include them in its library.

- **DateTime parsing counts:** Every native percentage now uses all observations
  as its denominator, including originally missing and failed values. The header names
  this denominator. This replaces undefined `NaN` cells and prevents the parsed share
  from being confused with the separate parse rate among non-missing values.

- **Interaction forest plots:** Both styles now show all fitted Cox coefficients with
  confidence intervals, explicit conditioning values and the model-table conditioning
  warning. Interaction rows are labelled as ratios of hazard ratios; non-estimable
  coefficients are labelled without a point or interval. Non-interaction forests retain
  their existing style. Recreate earlier interaction forest figures: a main-effect HR
  is conditional, and an interaction coefficient is not a subgroup HR. Model estimates,
  statistical methods and defaults are unchanged.

- **Adjusted survival curves with stratification:** Curves now use the exact Cox fit
  reported by the age-adjusted table, retaining the selected baseline strata, age
  strata or both. Every model-cohort patient is assigned to each group and predictions
  are averaged on a common time grid, ending at the shortest stratum follow-up.
  Drawing state contains aggregate coordinates and is invalidated by every
  stratification control. Saved curves from an earlier candidate require recalculation.
  **Recreate earlier adjusted-curve figures made with stratification selected.**
  The HR estimates and the existing adjustment choices are unchanged.

- **Further pre-submission review (2026-11-08):** Removed universal C-index grades and
  hazard-ratio benefit/harm labels; qualified the illustrative NNT and confidence-interval
  wording. Corrected weighted log-rank guidance to the implemented G-rho family. Clarified
  the adjusted-curve model mismatch and interaction-forest conditioning limitation, both
  subsequently repaired above. Refreshed release evidence and maintainer instructions.
  This documentation pass itself did not change estimates or statistical defaults.

- **Documentation review (2026-11-08):** Corrected missing-vital-status guidance,
  continuous age scaling per decade, the cutoff equality rule in validation examples,
  C-index range and interpretation, and distinctions between tuning and performance
  validation. Clarified composite endpoint times, fixed-duration versus calendar time,
  competing-risk estimands and model limitations. Added an executable methods/reporting
  vignette and release-review evidence. Analysis code, defaults and estimators are unchanged
  by this documentation pass.

- **Outcome Organizer maintenance:** Endpoint recoding, repeated-record hierarchy,
  interval preparation, administrative censoring and final output assembly now run in
  separate helpers. Event definitions, row handling, warnings and censoring behavior are unchanged.

- **Module audit repairs (2026-11-08):** Configured fatal inputs now raise framework errors in
  DateTime Converter, Outcome Organizer, Single-Arm Survival and Continuous Survival. Untouched
  selections retain guidance. Nine native result grids replace computed HTML grids across six
  analyses, preserving the estimates and counts. Turkish message extraction/catalog gaps and
  the identified English median/nomogram narratives are localized. Seven long run methods are
  split into ordered phases; the bootstrap formula fallback uses the shared safe parser.
  Statistical methods, defaults and clinical thresholds are unchanged.

This release follows an independent review of all nine analyses. Estimators agreed with reference
calculations (`survival`, `cmprsk`, `glmnet`, `logistf`); the defects were in how data reached them and
how results were labelled. **If you have reported results from an earlier version, read the first section.**

- **Saved plot state:** odds-ratio forest plots and continuous-survival cutoff plots retain
  metadata instead of patient records. Redraws and exports reconstruct the needed data from the
  dataset, and legacy plot states are cleared when redrawn.
- **R API input guidance:** survival-module wrappers accept a supplied data frame with no
  selected variables and return guidance. Regression tests now provide their translation binding
  and verify excluded-outcome counts against the fixture.

## Results from earlier versions that should be redone

- **Survival Analysis, adjusted survival curves:** the curves were not adjusted. Each group kept its own
  covariate mix, so a plot titled "Adjusted for age" reproduced the confounding it claimed to remove, and
  could contradict the age-adjusted hazard ratio beside it. Curves are now directly standardized.
  **Redo any figure made with Adjusted survival curves.** The hazard ratio tables were not affected.
- **Survival Analysis and Multivariable Survival, emptied reference level:** when exclusions left no
  patient in the first level of a variable (a marker never done on Grade 1, or a landmark past all of
  them), the hazard ratio was a different comparison, printed against a group with no patients, and
  could read in the reverse direction. Empty levels are now dropped and a note names the new reference.
  Check any table with an `NA (NA-NA)` row.
- **Survival Analysis and Multivariable Survival, Ordinal variables:** an Ordinal explanatory variable
  gave polynomial contrasts in unlabelled rows, which also produced a summary naming no predictor.
  Rows are now one hazard ratio per level against the first level, as for a Nominal variable.
- **Multivariable Survival, interaction models:** a main effect in an interaction model was described as
  an overall effect. It is the effect at the reference or zero value of the other term, and the output
  now says which, flags a value outside the data, and leaves such terms out of "strongest predictor".
- **LASSO Cox, with glmnet 5.0:** the cross-validation curve could be numerically impossible on a long
  lambda path, which could select the wrong lambda, including under the default rule. The path now stops
  before that point, and the output reports the lambda values used and searched. Models on healthy
  paths are unchanged.
- **Odds Ratio, separation:** detection read the printed text, so it could miss separation or flag a
  slope that was merely on a small unit. It now tests the fit. A separated term reads "not estimable",
  and the nomogram and Hosmer-Lemeshow test are withheld for it.
- **Survival Analysis and Multivariable Survival, dates stored as numbers:** a numeric day count was read
  as seconds with no message (a rate of 415559 per 100, a normal-looking Cox table). All analyses now use
  one check: numeric date columns are accepted only as epoch seconds and otherwise refused with the reason.
- **Survival Analysis (continuous), cut-off p-value:** the p-value at a data-derived cut-off ignored the
  search over candidate cut-offs. A new **Adjusted p** column corrects for it. The exported group log-rank
  p-value is unchanged and should not be reported on its own. With outcome-guided multiple cut-offs no
  citable correction exists, so the table labels p as unadjusted and the plot shows none.
- **Outcome Organizer, RFS, PFS, DFS and TTP:** these need a recurrence or progression variable and were
  built incorrectly or refused in several configurations. See below.
- **Outcome Organizer, DFS with a recurrence variable:** a patient coded Alive with Disease but recorded
  without recurrence was counted as an event. The recurrence variable now decides, and a warning lists
  the patients whose vital status and recurrence disagree so they can be checked.

## Changes across analyses

- **Landmark:** all analyses now keep patients with time strictly greater than the landmark. Single Arm
  already did; Survival Analysis, Survival Analysis (continuous), Multivariable Survival and the Time
  Interval Calculator kept patients at the landmark, so an exact tie (the norm with integer months) gave
  different patients in different tables. A negative landmark is refused, and landmark may be a decimal.
- **Dates:** a far-future "unknown" date such as 2199-12-31 is refused in every analysis; it used to halve
  incidence rates with an unchanged median in three of them. Unreadable dates are reported with a count
  everywhere, with the warning that a format that does not match the data can swap day and month in
  the dates that were read. Survival Analysis (continuous) no longer refuses a run when more than 20% of
  dates fail; that cut-off had no basis and no other analysis used it.
- **Saved columns:** Add Calculated Time and Add Outcome write every source row, before any landmark
  shift, in all analyses. Competing-risk outcomes are saved as a labelled Censored/Event/Competing factor,
  because a saved integer column reread as nominal treated competing deaths as censored.
- **Rates and proportions** are no longer rounded before display, and a 0% survival estimate stays in
  the table instead of being removed as extrapolation.
- **Time-point tables** are titled by what was asked (they read "1, 3, 5 year Survival" whatever the
  cutpoints were), with the unit in the column. The events column is "Events since previous time", and
  the sentence no longer presents that count as a running total.
- **Confidence intervals:** Survival Analysis and Survival Analysis (continuous) now use log-log
  (complementary log-log) intervals, as Single Arm does, so the same patients give the same intervals.
  Point estimates are unchanged; intervals shift slightly and stay inside 0 to 1. Tables say which method
  was used. **Time Type in Output** has one title across the four analyses.
- **Pre-calculated times** carry a notice that they were not converted, and a warning above 50 years.
- Variable names that clean to R keywords (`IF`, for example) or contain regular-expression characters
  (`Age (years)`) work in all analyses.

## Survival Analysis

- Age interaction uses a joint likelihood-ratio verdict. The REMARK checklist was rebuilt from the
  published 20 items (Altman et al. 2012). Calibration carries strata. Stale Cox and RMST results are
  cleared. Time-zero records are disclosed.
- RMST handles time horizons (`tau`) shorter than the earliest observed follow-up (`RMST = tau`, `SE = 0`)
  and bounds confidence intervals to `[0, tau]`. Restricted cubic splines (`rcs_analysis`) include the
  stratification variable when **Stratified Cox regression** is enabled. Competing-risks mode hides the
  Kaplan-Meier survival table when a 3-level competing-risks outcome from Outcome Organizer is used
  directly, and adds an explanatory footnote when cumulative incidence does not reach 50%.
- Known limit: adjusted curves under a stratified Cox model are not stratified, and the output says so.

## Survival Analysis (continuous)

- Hazard ratio digits follow the width of the confidence interval, so most markers now show 3 decimals.
- Incidence denominators are corrected, cut-offs print 6 significant digits, and quantile fallbacks
  are reported.
- Median follow-up no longer treats patients who died of another cause as still observed under
  Cause Specific.
- A marker column named `cutpoint`, `data` or `minprop` works.
- **Add Redefined Outcome to Data** writes every source row before landmark filtering, and the median
  and survival-table summaries report `"not reached"` instead of `"NA"` when a median or confidence
  limit is not reached.

## Single Arm

- Numeric dates use the shared check, and caveats about recoded outcomes and landmarks appear beside
  every affected result.
- Median follow-up uses the same reverse Kaplan-Meier denominator as the other analyses.
- An event at time zero is counted with no follow-up time, as in the other analyses, and a note says so.
  The person-time table used to be left empty.

## Multivariable Survival

- Competing risks with strata now runs. Standardized curves in a stratified model stop at the shortest
  stratum. A subgroup hazard ratio states the fixed value of the other term, and an empty cross-cell reads
  "not estimable". A single-level stratifier is refused.
- Fine-Gray competing-risks nomograms weight the covariate reference distribution (`datadist`) at the
  patient level rather than by expanded Fine-Gray counting-process rows, and risk-group cumulative
  incidence curves preserve `"Censored"` as the reference status level even when a risk group has zero
  censored observations.
- Interaction forest plots retain conditioning notes, reference values and all fitted
  coefficient rows; interaction coefficients are identified as ratios of hazard ratios.

## Odds Ratio

- Name collisions (level `2` of `grade` against a column `grade2`) and collinear predictors are refused.
- Hosmer-Lemeshow reads "not applicable" for a saturated model. The table states the shared complete
  cases. Outcome-window and cohort-sampling cautions are added. An infinite likelihood ratio gives an empty
  cell with the reason. A column named `LP` works.

## LASSO Cox

- The output states the units of coefficients and always reports that selection depends on the fold split.
  The coefficient-path plot has a caption.

## Outcome Organizer

- RFS, PFS, DFS and TTP work under Multiple Event Levels. Without a recurrence variable they are refused,
  except DFS from vital status alone, which stays under Multiple Event Levels with a warning.
- DFS built from vital status alone (no recurrence variable) counts Alive with Disease as an event,
  matching Survival Analysis. With a recurrence variable, recurrence decides, and disagreements are
  listed for checking.
- A multi-level outcome with one Event Level warns and lists the levels coded 0 with their counts. A
  recurrence variable with more than two levels warns and names the others.
- Blank IDs no longer share events. A cut-off on a recurrence endpoint warns that Follow-up Time must be
  the time to that event. A column already coded Censored/Event/Competing is refused as the outcome of
  RFS, PFS or DFS. The saved status is an integer on every path.
- Event hierarchy (`useHierarchy`) leaves missing outcomes (`NA`) as missing instead of recoding them
  to censored (`0`) or the priority event, and censors undated duplicate priority rows after retaining
  the dated priority event. Multi-state mode populates the Outcome Recode summary card.

## Date-Time Converter and Comprehensive Time Interval Calculator

- `DD/MM/YYYY HH:MM` reads correctly. The calculator gains `dmy`, `mdy` and `ymd` with hours and minutes
  or seconds. Six-digit dates under Auto are dates, and Excel serials stay serials. "Always in the past"
  works on packed two-digit years. A midnight daylight-saving gap (Istanbul, 1964-05-15) no longer
  loses the date.
- `Date` columns in Date-Time Converter preserve their calendar day across Western Hemisphere timezones
  instead of shifting back by one day at UTC midnight, and implausible-year warnings (`< 1900` or future
  years) apply to pre-formatted `Date` and `POSIXct` columns as well as text and numeric dates.
- Year-month text is set to the 1st with a warning. An explicit impossible format leaves rows empty and
  says why. The calculated column keeps its unit and landmark name.
- The calculator withholds the copy-ready sentence and the Good grade while day/month order is unsettled.
- **Time basis** now defaults to calendar months, the months the survival analyses compute from the
  same dates. An exact year used to read 12.02 months here and 12 there. Standardized 30.44-day months
  remain available.

## Example data

- `lassocox_lung_cancer`, `lassocox_genomic` and `lassocox_small_cohort` were regenerated with realistic
  censoring. The first two had 2 censored patients and the third 3, and two had zero follow-up times,
  so LASSO Cox could not fit them.

## Earlier changes in this version

### Behavior changes: saved analyses may produce different results

- **Odds Ratio:** new **Negative Level** and **Predictor Negative Level** options name the
  reference outcome and the negative test result explicitly. Both outcome levels must be chosen;
  any other outcome levels are excluded instead of being pooled.
- **Odds Ratio:** the Hosmer-Lemeshow test uses the number of risk groups actually formed
  (number of groups minus 2 degrees of freedom). It used 8 even when tied predictions formed only 3 groups, which made
  calibration look falsely reassuring for all-categorical models.
- **Comprehensive Time Interval Calculator:** when two date formats read the dates equally well,
  auto-detect now drops readings whose years span more than 20 (the rule Date-Time Converter
  uses) instead of preferring the narrowest year range, which could pick day-month-year for
  year-month-day dates.
- **Multivariable Survival:** the optimism-corrected C-index keeps bootstrap resamples whose Cox refit
  only warned (for example monotone likelihood); dropping them biased the corrected C upward when
  events per variable were low. The table says how many resamples were used and why any were
  excluded.
- **Survival Analysis:** with stratified Cox and age adjustment both on, the age table's unadjusted
  hazard ratio now uses the same strata as the adjusted one. The calculated-time output column holds
  every row's date-derived time, before any landmark shift.
- **Date-Time Converter:** with a text date format selected, whole numbers are read as digits
  (`150321` as 15-03-21, restoring a lost leading zero when that is the only valid reading) instead of
  as Excel serial numbers. Dates converted from serial numbers that fall outside a plausible range are
  flagged and not graded.
- **Comprehensive Time Interval Calculator:** invalid data (for example an end date before its start
  date) now stops the analysis with jamovi's error message instead of returning empty results with an
  error banner. The data-quality grade accounts for mixed day/month order and other warnings.

### New and fixed

- **Survival Analysis:** a stratification variable whose name contains braces (for example
  `Stage {I-IV}`) no longer breaks the model summary; such names were interpreted as code. Pairwise,
  weighted log-rank and bootstrap titles name the variable instead of showing "NA". The copied R
  syntax runs when optional event levels are unset.
- **Odds Ratio, LASSO Cox and Outcome Organizer:** the copied R syntax runs when level options are
  unset. Outcome Organizer's disease-free survival panel no longer mentions a category that does not
  exist on that path.
- **Survival Analysis (continuous) and Multivariable Survival:** remaining blank-instead-of-"NaN"
  cases (time points beyond a group's follow-up, an RMST without a standard error, aliased interaction
  terms); "Analysis complete" lists only steps that produced results.
- **Panels that should be empty are cleared:** several analyses left the previous run's text on
  screen when a run had nothing to report (jamovi does not redraw an empty panel).
- **Single Arm:** a landmark that excludes nobody is still disclosed.
- **Odds Ratio:** a saved analysis keeps only the drawn prediction nomogram (its axis scales), no
  longer the patients' model data; the nomogram is redrawn from it when the file is reopened.

- **Odds Ratio:** terms whose odds ratio is not estimable (separation) are left out of the forest
  plot, with a caption naming them. A table note names the confidence-interval method used in the
  run, including when profile-likelihood intervals fall back to Wald. The C-statistic is labelled
  in-sample, and a numeric 0/1 variable is accepted as the diagnostic predictor.
- **Outcome Organizer:** the summary and copy-ready text name exactly which outcome levels are coded
  1 and 0 (single-level overall survival no longer calls other-cause deaths "alive"); unset levels
  read "not set" instead of empty brackets; Time to Progression no longer asks for an Event Level it
  does not use; the plot lists codes in order.
- **LASSO Cox:** coefficient and importance rows read "Grade: Grade 3 (vs Grade 1)" instead of
  design-column names; the refit test p-value reads "p < 0.001" instead of "p = 0.000".
- **Date-Time Converter and Time Interval Calculator:** values with text after the date are reported
  instead of silently truncated; Date-Time Converter names daylight-saving gaps and suggests
  Auto-detect for epoch milliseconds; a two-digit-year warning is kept when the interval check
  rejects the run; the copy-ready sentence says it reports observed time and states the month and
  year lengths.
- **Survival Analysis (continuous):** an unsupported RMST horizon is reported once; ignored survival
  time points are reported; a time point beyond a group's follow-up or one that fails is shown blank
  with a footnote instead of disappearing; a failed log-rank test is noted.

- **Comprehensive Time Interval Calculator** is back in this module (Survival > Data Preparation).
- **LASSO Cox:** a factor predictor carrying an explicit `NA` level (for example from `addNA()`) is
  treated as missing instead of as a category of its own. The generated R syntax keeps every
  option the analysis ran with.
- **Date-Time Converter:** Unix epoch values in milliseconds are detected and converted, with a
  note saying so.
- **No more "NaN" cells:** a median that is not reached, and a confidence limit or p-value that is
  not computed, are now blank with a footnote or table note saying why (Survival Analysis, Single
  Arm, Survival Analysis (continuous), Multivariable Survival). The always-empty Schoenfeld column
  of the case-level residuals table is removed; those residuals stay in the event-time table.
- **Survival Analysis (continuous):** with quantile cut-offs, a minimum group size that the
  requested quantiles cannot meet now falls back to fewer equal-sized groups (for example tertiles)
  instead of an uneven split. The survival-plots explanation now appears only when **Show
  explanations** is on.
- **Survival Analysis:** in competing-risks mode the Kaplan-Meier survival table is no longer shown
  empty; the median table says where the cumulative-incidence time-point estimates are when that
  table is off. Age-adjusted outputs appear only when **Age-adjusted Cox regression** is on (they used to show
  empty). The log-log plot draws the Kaplan-Meier steps at the observed event times.
- **p-values read "p < 0.001" or "p = 0.042"** instead of "p=<0.001", "p = < 0.001" or
  "p = 0.000" (Survival Analysis, Survival Analysis (continuous)). The continuous-variable Cox text
  shows bold headings instead of literal `**` marks.
- **Multivariable Survival:** the clinical summary names each term as entered ("treatment: B
  (vs A)") instead of the internal coefficient name ("treatmentB").
- **Single Arm:** the time-point summary keeps the capitals in "Kaplan-Meier".
- **Survival Analysis (continuous), Turkish:** with fewer than 10 non-missing predictor values the
  multiple cut-off analysis now gives the minimum-count message instead of a formatting error.
- **Multivariable Survival:** the plain-language summary names the interaction terms in the model,
  and refreshes when the summary or plot options change.
- **American spelling throughout** (analyzed, modeled, standardized, behavior, labeled). The Turkish
  translations follow.
- **Package:** `ggpubr`, used for the risk table under the Single Arm survival plot, is now declared
  in Imports instead of arriving through survminer.
- **Repository:** a personal settings file is no longer tracked.

# jsurvival 1.1.1 (2026-10-21)

The first release since 1.0.81. It also adds observed-time competing-risk options (cumulative
incidence) to Survival Analysis and removes the Time Interval calculator from this module; the
rest of this entry covers the development builds 1.0.82 to 1.0.82.03. It stops LASSO Cox from guessing which outcome value is the event, puts saved risk scores back on the
right patients when a filter is active, and repairs `multisurvival` plots and data columns that went
blank or stale.

## Behaviour changes -- saved analyses may produce different results

- **`lassocox`: the event is never guessed from the outcome's coding.** With Event level left empty,
  1.0.81 took the second level of a text outcome as the event (so `Died`/`Survived` made *Survived*
  the event) and the larger value of a numeric one (so a 1/2 column always made 2 the event). Only a
  numeric 0/1 outcome is now read without asking (1 = event); any other coding stops, lists the
  values found and asks you to choose the event. Saved analyses that relied on the guess stop until
  Event level is set. An empty Censored level still means "the other observed value".
- **`lassocox`: saved risk scores land on the right patients when a filter is active.** "Add Risk
  Score to Data" wrote the scores by position, so with any jamovi filter in place they shifted onto
  other patients' rows -- 68 of 68 in a test filtered on ER status. If you saved risk scores from a
  filtered dataset, run the analysis again and re-save the column.

## Fixed

### `multisurvival`

- A numeric outcome coded 1/2 (as in the widely used `lung` dataset) was refused as "Outcome should
  be binary" even after its event value had been chosen. It now runs, and a note states which value
  was read as the event and which as censored.
- The Adjusted Probability Plot and Brier Score Over Time went blank, with "Data contains no
  (complete) rows", when the output was resized, a saved `.omv` file was reopened, or results were
  exported. Both now redraw from the results already computed.
- "Add Calculated Time to Data" and "Add Redefined Outcome to Data" kept their first values after
  the time unit, date settings, landmark, outcome coding or model variables changed; they now
  refresh. In competing-risk mode the redefined outcome is created as a Nominal column, so its
  Censored/Event/Competing labels survive.
- Invalid input (no complete rows, an outcome that cannot be coded) shows jamovi's normal error
  message instead of a generic "Survival Analysis Error" box, and the Optimism-Corrected C-index
  table no longer keeps the previous run's numbers under a new error. An explanatory variable with a
  single value in the analysed rows is named in the message instead of failing with an R error.
- New table notes: within-subgroup hazard ratios are exploratory and unadjusted for multiple
  comparisons, and an effect that is significant in one subgroup but not another is not evidence
  that it differs (use the interaction test); the Covariate Contribution tests are unadjusted for
  multiple comparisons; the bootstrap seed is shown under the Optimism-Corrected C-index.
- Smaller: one events-per-variable notice after the fit instead of an extra low-events warning
  before it; "Patients in the model" replaces "Total patients"; an empty "Adjusted Probability
  Natural Language Summary" heading is gone.

### `lassocox`

- Validation messages show as jamovi's standard error instead of an "Analysis Error" box that also
  emptied the summary tables. When the selected rule keeps no predictors, the coefficient plot says
  so instead of staying blank. The Random seed box moved into Model Options, and the seed used is
  printed under the Model Summary.

### `survival` and `singlearm`

- `survival`: the bootstrap-validation seed is printed under its table; the calibration and
  parametric-model comparison tables show all their rows from the start, and a distribution that
  cannot be fitted keeps a blank row instead of vanishing.
- `singlearm`: the Data Quality table keeps a fixed set of rows in a fixed order and no longer shows
  an earlier run's values when a new run stops early.

### All analyses

- Sentences assembled from several pieces no longer start with a stray space or comma, which broke
  word order in the Turkish translation (`survival`, `survivalcont`, `timeinterval`,
  `datetimeconverter`, `outcomeorganizer`).

## Example data, documentation and packaging

- Six example datasets for LASSO Cox are bundled for use from R: `lassocox_breast_cancer` (250
  patients, 22 variables), `lassocox_cardiovascular`, `lassocox_genomic` (80 patients, 56
  variables), `lassocox_lung_cancer`, `lassocox_multicollinear` and `lassocox_small_cohort`. The
  `lassocox()` help page shows an example using the first.
- The eight `stagemigration_*` example datasets are removed; they belong to the stage-migration
  analysis, which this module does not ship. `histopathology`, `melanoma`, `data_longitudinal` and
  the example analysis in jamovi's data library are unchanged.
- The README now describes the nine analyses this module actually contains, with installation steps
  and R examples. The documentation website build is automated, the module has its own icon, and
  the citation metadata gives the licence as GPL-2.0-or-later, matching the package's `GPL (>= 2)`.

# jsurvival 1.0.81 (2026-09-09)

This release adds LASSO Cox regression and a Turkish translation, and carries a correctness review
of every analysis in the module. Many of the defects produced plausible wrong numbers rather than
errors -- dates read a century or a day/month swap out, an administrative cut-off that never
censored anyone, a stratified Cox model that was not stratified -- so behaviour changes come first.

## Behaviour changes -- saved analyses may produce different results

- **`survival`: a stratified Cox model is now stratified.** With a stratification variable selected,
  the hazard-ratio table came from an unstratified model while the output called it stratified. The
  table and the proportional-hazards test now use the stratified model.
- **`survival`: a numeric explanatory variable with fewer than 5 distinct values is analysed as
  groups everywhere.** The Cox table treated it level by level, but the proportional-hazards test and
  the age-adjusted models fitted one linear trend, so a report could show 1.02 and 1.12 per level
  beside 1.06 per unit for the same variable. Every output now uses the group coding and says so.
- **`oddsratio`: numeric predictors are entered as typed, as a linear trend.** A numeric predictor
  with fewer than 5 distinct values was split into levels in the main table but fitted as a trend by
  the Firth option and the nomogram, so ticking Firth changed the model, not just the estimator. Odds
  ratios, AIC and C-statistic change for such predictors; set the variable to Nominal or Ordinal for
  level-by-level estimates. A note names each predictor entered as a trend.
- **`singlearm`: confidence intervals use the log-log method**; they stay inside 0-100% instead of
  being clipped at 100%, and are blank where the estimate is exactly 0% or 100%. A
  cumulative-incidence "median" reached only at the last observed time, with one subject at risk, is
  no longer reported (the bundled example showed "median 202.8 months" beside "18% had the event").
- **`survivalcont`: the minimum-p-value search for multiple cut-offs is reproducible.** It sampled
  1,000 random combinations without a seed, even when fewer existed. It now tries every combination
  when there are 1,000 or fewer, otherwise 1,000 distinct ones drawn from the Random seed, enforces
  the minimum group size during the search, and says when the answer is approximate. The default
  RMST horizon is limited to follow-up that every displayed group reaches.
- **`multisurvival`: the optimism-corrected C-index is reproducible.** The bootstrap had no fixed
  seed, so each run gave a slightly different value. A new **Bootstrap seed** option (default 1234)
  fixes it. Interaction terms no longer appear in the univariable HR column, where each was fitted
  without its main effects; the row stays, with "-" in that column.
- **`timeinterval`: two date errors that inflated follow-up are corrected.** Two-digit-year
  `DD/MM/YY` columns were read year-first ("22/09/18" as 2022-09-18): on a 30-patient cohort, mean
  follow-up 3,775 days instead of 493. Date columns exported from a spreadsheet as plain numbers
  (42370, 42371, ...) were split into digits, giving a 12-month cohort a mean of 792 months; they are
  now refused with an explanation.
- **`timeinterval`: "Add calculated times to dataset" now adds a column** -- it never did -- named
  with its unit and any landmark, e.g. `Calculated Time (months)`. Saved files that had the old box
  ticked reopen with it unticked, and the R argument `add_times` is gone.
- **`datetimeconverter`: four silent date errors corrected.** Excel times were a minute early on
  about 11% of timestamps (00:05 became 00:04:59.999); the Timezone option did nothing for columns
  already stored as date-times while the summary said it had been applied; two-digit-year day-first
  dates (`05/03/21`) were read year-first (2005-03-21); and a single negative value such as a `-99`
  code moved a whole Excel column to the 1904 date system, about four years off. Negative values in
  an Excel column are now treated as missing.
- **`outcomeorganizer`: administrative censoring now censors.** It compared a follow-up duration with
  a date stored as a day count (18992 for 2021-12-31), so the cut-off never fired: in a 30-patient
  test all 15 deaths stayed events while the Summary said follow-up had been censored. Durations and
  dates on different scales are now refused, showing both numbers. Under the multistate coding,
  administrative censoring and the event hierarchy wrote patients into "alive without disease"; both
  now decline to run on a coding that has no censored state.

## New

- **LASSO Cox regression (`lassocox`)**, under Survival > Penalized Cox Regression > LASSO Cox, for
  exploratory selection of predictors from a candidate set. Lambda is chosen by minimum
  cross-validation error or the 1-SE rule (default) over 10 folds, reduced when needed so every fold
  holds events and censored cases, with a seed for reproducible folds. Output includes a
  data-suitability check, penalized coefficients, cross-validation, coefficient and path plots,
  development-sample risk-group curves, a risk score that can be added to the data, an optional
  unpenalized refit, the predictor encoding, the exact lambda/seed/folds, and R code that reruns the
  fit outside jamovi. Performance is apparent -- measured on the data that built the model -- and
  labelled so; the model needs internal and external validation before clinical use.
- **Turkish translation.** All of the module's interface and results text (about 2,700 strings) is
  available in Turkish through jamovi's language setting.
- `datetimeconverter`: a **Two-digit years** option -- *Always in the past* moves dates that would fall
  after today back a century (167 of 200 test dates of birth had landed in 2035-2068) -- and an
  **Excel Serial (Days since 1904, legacy Mac)** format for Mac Excel files, which were about four
  years off with no way to correct them.
- `outcomeorganizer`: **Add Truncated Follow-up Time to Data**, so a patient censored at the cut-off
  keeps the shortened follow-up beside the recoded status rather than the original one.

## Fixed

### `survival`

- "Export Estimated Survival" added no column, beside a summary saying it had. It now adds each
  patient's Kaplan-Meier probability at their own follow-up time, from their own group's curve.
- The age-stratified plots were blank in date mode and with multiple outcome levels, ignored the
  landmark, and the faceted age-group plot never drew. They now use the same data as the tables.
- In competing-risk mode Gray's test is reported as the group comparison. Calibration uses the
  baseline hazard at or before the chosen time; the nearest step could lie later and over-predict risk.
- The residuals table drops its Schoenfeld column (those residuals exist only at event times; the
  proportional-hazards plot shows them), and the residual plot is drawn against the linear predictor.
- Groups with zero events are named, with why their median and hazard ratio cannot be estimated; with
  two groups the pairwise table stays, with a note; the person-time explanation is no longer
  overwritten.

### `multisurvival`

- Nomogram: a risk axis for every requested time point (only the first was drawn); a single predictor
  or a narrow predicted-risk range no longer makes it fail silently; it redraws after resizing.
- A proportional-hazards violation is a strong warning naming the terms and p-values; convergence
  problems appear in the results, not only in Analysis Notes; three overlapping small-sample warnings
  are folded into one graded events-per-variable notice.
- The Person-Time Analysis table shows per-group rows for variables whose names contain capitals or
  spaces; table headers show your variable names, not internal ones.
- Notices when the plot end time cuts curves short of the follow-up (the default of 60 is meant in
  months) and when a requested time point lies beyond follow-up; in competing-risk mode, competing
  events no longer shorten the median follow-up; the C-index carries no verdict word.

### `survivalcont`

- Renamed in the menu: **Survival Analysis for Continuous Explanatory Variable**.
- The Cox summary displayed raw HTML tags; it is now formatted text. "Calculated survival time"
  exports the real interval, not the landmark-shifted one.
- The short-follow-up warning uses the reverse Kaplan-Meier median follow-up, so it no longer fires on
  well-followed cohorts with many early events.
- The single cut-off's p-value is no longer described as adjusted, a false multiple-comparisons
  warning based on the number of plots is gone, sparse Cox strata are warned about, and censor marks
  and the median line now apply to the multiple cut-off plot.

### `singlearm`

- The KMunicate-style plot was a blank panel in every configuration; it now draws.
- Zero or few events of interest raise a notice with the counts -- zero events usually means the wrong
  event level, which otherwise reads as 100% survival.
- Headings follow the survival type ("Median disease-free survival"); in competing-risk mode the table
  states it reports cumulative incidence, not 1 minus Kaplan-Meier. The piecewise hazard table no
  longer stays hidden after switching back from competing risks.

### `oddsratio`

- The forest plot lost its estimates for numeric predictors with few values (labelled rows with no
  odds ratio, plus an unlabelled row holding the only one); it now matches the table.
- Headers show the outcome as selected ("Dependent: Disease Status"), not an internal name. The
  diagnostic 2x2 table is positive-first, so its cells read TP, FP, FN, TN as printed beneath it.
- A numeric value carried by fewer than 5 observations is warned about; progress messages from ROC and
  profiling packages no longer reach Analysis Notes.

### `timeinterval`

- Rows typed in the opposite day/month order raise a strong warning when their follow-up differs from
  rows that can only be read one way, and the Data Quality panel states how many rows and how much
  person-time could have been typed either way. The Clinical Summary is withheld while dates are in
  doubt.
- Landmark analysis: durations are labelled as measured from the landmark; the glossary no longer says
  it selects "6-month survivors" (with no event indicator it selects on length of follow-up); a
  landmark past the median interval, or one that excludes nobody, is explained.
- "Flag extreme values" deleted rows (41% of person-time in one test). It is renamed **Remove extreme
  values**, states its threshold and count, and warns when rows go; removing negative intervals warns
  at any count and escalates at 10%.
- The confidence interval for mean follow-up no longer dips below zero ("-0.04 to 0.12"); `n = 1`
  warns; the summary no longer says "Missing values: 0" under a banner reporting ten; the person-time
  panel no longer claims to account for censoring; messages run most severe first.

### `datetimeconverter`

- Numeric columns that are not dates -- ages and scores becoming dates around 1900, platelet counts
  dates in the 2300s -- now raise a warning, as do Excel serials fed to the Unix-epoch format (every
  value collapsed to one day in 1970).
- A `dd/mm/yy` column whose day is always the 1st or 15th was read year-first (`01/05/19` as
  2001-05-19); a genuinely ambiguous column now shows both readings of one of its own values.
- "Success rate" is now **Parse rate**, with no grade while a notice questions the dates; the
  copy-ready sentence uses one denominator (it could read "50/100 (100%)"); a format matching exactly
  80% of sampled values is accepted (32 of 40 were discarded).
- A numeric format chosen for a text column is explained instead of failing with an R error; a Date
  column converted to date-time keeps midnight in the chosen time zone; Week number is labelled as
  not the ISO week.

### `outcomeorganizer`

- A three-level registry outcome (alive / dead of disease / dead of other causes) could not be used for
  competing-risks or cause-specific survival without inventing a fourth category. Every level present
  must be assigned, but an empty category is allowed.
- Priority Event Type 0 erased every event; the minimum is 1, and a priority code absent from the data
  is reported as such instead of "applied".
- The exported column is named e.g. "Recoded Outcome (overall survival)", not "Recoded Outcome for os
  Survival Analysis"; re-running on that column no longer labels competing events "Unknown (2)".
- The copy-ready text describes each analysis type's own coding; composite RFS/PFS/DFS endpoints
  warn that they carry no time to first event; losing most of the cohort to missing outcomes is a
  strong warning.
- Interval censoring with start after end, or endpoints on different scales, is refused; ticking
  interval or administrative censoring without its variable is reported; a date-time cut-off no
  longer moves a day west of Greenwich.

### All analyses

- `survival`, `singlearm` and `oddsratio` keep result panels hidden until the required variables are
  chosen, and `survival` shows invalid input as jamovi's own error message.
- With explanations on, `singlearm` and `multisurvival` say how median follow-up was estimated
  (reverse Kaplan-Meier) and why it differs from the median observed time.
- Coloured explanation panels are readable in jamovi's dark theme. Option labels are in sentence case
  and lose redundant verbs ("Survival time from dates", "Risk table", "Censor marks", "Median line").

## Packaging

- Nine R packages no shipped analysis used are no longer installed with the module (MASS, maxstat,
  pec, purrr, randomForestSRC, rpart.plot, survivalsvm, SurvMetrics, xgboost); Matrix is added for
  LASSO Cox.

# jsurvival 1.0.6.03 (2026-08-22)

This release corrects interpretation text that was statistically wrong, removes clinical advice from
results, and stops third-party package notices appearing in the results pane. One defect produced a
garbled and incorrect sentence in the most-used analysis and is listed first.

## Behaviour changes -- saved analyses may produce different results

- **`survival`: the Cox summary sentence no longer states a hazard ratio as a risk ratio, and no
  longer prints a whole formatted cell where a number belongs.** The natural-language Cox summary
  interpolated finalfit's formatted output -- a string such as `1.50 (1.10-2.05, p=0.010)` -- into a
  sentence expecting a single value, producing *"there is 1.50 (1.10-2.05, p=0.010) times risk than
  when Grade is 1"*. Beyond being unreadable, "times risk" is wrong: a hazard ratio is a ratio of
  instantaneous event rates among those still at risk, not a ratio of cumulative risks. The sentence
  now reads as an estimated hazard ratio relative to the reference group, states what a hazard ratio
  is, notes the proportional-hazards assumption it carries, and explains that a confidence interval
  including 1 means the data are compatible with no difference in hazard.

## Fixed -- statements that were not true

- **`survival`: a non-significant interaction test no longer establishes consistency.** Failing to
  reach p < 0.05 in an interaction test was reported as the effect being consistent across
  subgroups. Interaction tests in survival data are markedly underpowered, so this is absence of
  evidence rather than evidence of absence; the text now says so.

- **`survival`: the p-value explanation is correct.** The panel described p < 0.05 in terms that
  implied the probability the finding is due to chance. It now states what the p-value is computed
  under.

- **`multisurvival`: the landmark caveat is present.** With landmark analysis enabled, every patient
  whose follow-up ended before the landmark is dropped and the clock is restarted at the landmark.
  Neither was disclosed, so a cohort could shrink and a time origin move without explanation. Both
  are now stated where the results are shown.

## Changed -- results no longer give clinical advice

- Text that told the reader what to do with a patient, or ruled on whether a model was fit for
  clinical use, has been replaced by explanation of what was estimated and how to read it. Guidance
  about running the analysis itself -- cohort size, assumption checks, which comparison is being
  made -- is unchanged and remains.

## Fixed -- output hygiene

- **R package chatter no longer appears in Analysis Notes.** jamovi surfaces `message()` and
  `warning()` conditions to the user, so notices from survival plotting and modelling packages
  leaked into results. Third-party calls in `survival`, `multisurvival`, `survivalcont`, `singlearm`
  and `oddsratio` are now wrapped so package chatter and deprecation notices are suppressed, while
  substantive warnings such as non-convergence still reach you.

- **A malformed reference no longer prevents results from rendering.** A citation with an empty
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

- **Option coverage went from 78% to 95%.** Of the 304 options across the eight shipped analyses,
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
- **`survivalcont` was shown with a `padjustmethod` argument.** No such option exists, or ever has.
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
  wearing a group label.** They came from the model's common risk set, so they were identical for
  every level of the adjustment variable while the narrative described them as group-specific.
  The columns are now titled for what they are, and the estimand is stated above the table.
- **"Adjusted Cox Model Results" was a different model from the main table.** In one real run the
  main multivariable table showed `performance_status` as a factor with two rows and 13
  likelihood-ratio degrees of freedom, while the adjusted table showed it as one continuous row
  with 12 df -- two models fitted, both displayed, nothing saying so. One code path honoured the
  column's factor type and the other coerced it to a score. Both now derive from the same fitted
  object. (`multisurvival`)
- **Competing-risk detection ignored a recoded outcome column.** The guard read
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
- **`singlearm` multi-event level validation missed unset levels.** The check collected the four
  level options with `c()`, which silently drops `NULL`s, so a missing assignment went undetected
  instead of being reported. Each level is now checked individually.
- **`singlearm` named the wrong confidence-interval method for the median.** The note said
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

- **`survival` reported survival probabilities beyond the observed follow-up.** The 1/3/5-year
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
- **`survival` told users to judge significance by eye from two confidence intervals.** The
  interpretation panel stated "Non-overlapping confidence intervals suggest significant
  differences" as a decision rule, alongside "Use for patient stratification and treatment
  decisions". Overlapping intervals do not imply the absence of a difference; the text now points
  to the log-rank test and drops the treatment-decision claim.
- **`multisurvival` reported no joint test for a multi-degree-of-freedom interaction.** The
  interaction table gives one row per coefficient, each a 1-df Wald test. For a 3-level x 2-level
  interaction that is two rows -- p = 0.077 and p = 0.726 -- and neither answers "is the effect
  modified by this variable at all?" The joint 2-df test over the same model is p = 0.154. It is
  now reported alongside the rows (Wald, agreeing with the likelihood-ratio test to three
  decimals: chi^2 = 3.7426 / 2 df / p = 0.1539 against 3.7466 / 2 / 0.1536).
- **`multisurvival` person-time could show figures from a previous covariate set.** Person-time is
  computed on the complete-case set across *all* selected variables, so adding one continuous
  covariate with missing values moved the overall row from 134 events / 8235.5 person-time to
  94 / 6005.47 -- but `contexpl` was absent from that table's `clearWith` and the table never
  cleared its rows. Both are fixed.
- **`oddsratio` printed a non-estimable odds ratio as a precise number.** Under (quasi-)separation
  `glm`'s IRLS halts wherever the iteration limit leaves it, and the table rendered
  `118848049086800030859264.00 (0.00-Inf, p=1.000)` -- an odds ratio of 1.19 x 10^2^3 shown to two
  decimal places. The cell now reads `not estimable`, with a strong warning naming the variable
  and pointing to Firth penalized regression. The rule keys on the confidence interval being
  unbounded, so a large but genuinely estimable odds ratio is untouched.
- **`oddsratio` reported diagnostic metrics with no uncertainty.** Sensitivity, specificity and
  the likelihood ratios were bare point estimates: 63.8% from 20 patients and from 2000 read
  identically. They now carry Clopper-Pearson exact intervals for the proportions and the log
  method (Simel et al. 1991) for the likelihood ratios -- reproducing `epiR::epi.tests()`
  bit-identically (maximum absolute difference 0 across 4,156 two-by-two tables). The arithmetic
  is adapted from epiR (GPL >= 2, credited in the source) rather than imported, so `jsurvival` does
  not gain an epidemiology package as a dependency for one call.
- **`oddsratio` named the wrong confidence-interval method.** The footnote said "Wilson score";
  `epi.tests()` defaults to `method = "exact"`, which is Clopper-Pearson. Wilson differs in the
  third decimal.
- **`datetimeconverter` warned "Implausible Dates Detected" on every successful conversion.** A
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
- **`datetimeconverter` documentation contradicted the tool.** The glossary gave Excel serial
  45000 as "May 18, 2023"; it is **15 March 2023**, which is what the converter itself returns.
  The Excel entry now also explains the 1899-12-30 origin and the 1900 leap-year quirk.
- **`datetimeconverter` claimed its numeric output was timezone-independent.** A date with no time
  is midnight *in the selected zone*, so `2024-01-15` becomes 1705276800 under UTC and 1705266000
  under Europe/Istanbul. With the default ("system") two collaborators on different machines get
  different numbers from the same file. The option text now says so and tells you to select UTC
  when the value must be reproducible.
- **`timeinterval` could write the calculated column back with no row mapping.**`self$data` holds
  only the columns the analysis asked for, so it has one column whenever the same variable is
  chosen as both start and end date; subsetting without `drop = FALSE` collapsed it to a vector,
  `rownames()` returned `NULL`, and `setRowNums(NULL)` then misaligned the written column.
- **`timeinterval` extreme-value filtering deleted genuine follow-up.** The rule is
  "> multiplier x 99th percentile", which only orders correctly for a positive 99th percentile. In
  a cohort where 99% of patients enter and exit on the same day the threshold is 0, so every
  non-zero interval counted as extreme and the handful of real follow-ups were dropped. The filter
  is now skipped in that case, with the reason stated under "Filters applied".
- **`outcomeorganizer` lost the competing-risk flag when no competing event was observed.** The
  recoded column was exported as a character vector, and `jmvcore` derives a column's levels from
  the values present -- so a competing-risks run on a cohort without other-cause deaths shipped a
  column declaring only `Censored`/`Event`. Downstream, `survival` and `multisurvival` identify
  this interchange format by requiring all three declared levels, so the hand-off was not
  recognised: the analysis stopped with "Event Level is not selected", or ran as plain
  Kaplan-Meier if the user picked "Event" to get past it. The column is now exported as a factor
  declaring `Censored`/`Event`/`Competing` whether or not all three occur, which also puts the
  levels in the order the 0/1/2 codes imply rather than alphabetically.

## Changed

- **Numerical verification tests now verify numbers.** The six `test-*-verification.R` files
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

- **Automated GitHub release (`.github/workflows/release.yaml`).** A push to the default branch
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
