# Changelog

## jsurvival 1.0.0 (2026-07-13)

### First stable release

- Fixed both odds-ratio validation paths so empty or invalid data
  produce clear analysis-level errors instead of an R6 method failure.
- Replaced non-structural named HTML entities with Unicode and converted
  fragmented translated output to complete placeholder-based sentences.
- Normalized checkbox labels to noun phrases, removed stale `sas`
  invalidation keys, and hid unfinished survival-tree controls until the
  feature is wired end to end.
- Trimmed `00refs.yaml` to the 22 citations used by the eight analyses
  and replaced broad package imports with selective imports.
- Synchronized `DESCRIPTION`, jamovi metadata, and `CITATION.cff`;
  removed the orphaned survival-power event handler.

## jsurvival 0.0.47 (2026-07-05)

### Bug Fixes

- **Fixed a crash in competing-risks analysis on jamovi installs.**
  [`cmprsk::cuminc()`](https://rdrr.io/pkg/cmprsk/man/cuminc.html) is
  called by the survival, single-arm, continuous-survival, and
  multivariable-survival analyses, but `cmprsk` was missing from the
  package `Imports`. Because jamovi installs only a package’s `Imports`,
  competing-risks analysis failed on a clean install with “there is no
  package called ‘cmprsk’”. `cmprsk` is now declared.
- Declared `digest`, `pec`, and `rpart.plot` (previously used via `::`
  but undeclared). Moved the optional machine-learning survival backends
  (`randomForestSRC`, `xgboost`, `survivalsvm`, `SurvMetrics`) to
  `Suggests` so they degrade gracefully when not installed.

## jsurvival 0.0.46 (2026-07-04)

*This release consolidates versions 0.0.32.62 through 0.0.46 into a
single entry. Headline themes: a major expansion of the univariate
**Survival** module toward REMARK-compliant prognostic reporting
(age-adjusted analysis, a weighted log-rank test family, calibration
assessment, non-linearity testing, and bootstrap internal validation);
Cox interaction / effect-modification terms in **Multivariable
Survival**; Firth penalized logistic regression in **Odds Ratio**; and a
unified multi-tier HTML notice system across modules. Minimum jamovi
version raised to 2.7.27.*

### New Statistical Features

#### Univariate Survival (`survival`)

- **Age-adjusted survival analysis** — new option group controlling age
  adjustment (`age_adjustment`, `age_variable`):
  - `age_interaction` — test an age x group interaction (new
    `ageInteractionTable` with coefficient, HR, SE, z, p-value).
  - `age_stratified_cox` and `ageAdjustedCoxTable` comparing unadjusted
    vs. age-adjusted hazard ratios side by side.
  - `age_time_scale` — fit a Cox model using age as the time scale (new
    `ageTimeScaleTable`).
  - `age_standardization` with `age_standardization_method`
    (`indirect`/SMR or `direct`) producing an `ageStandardizationTable`
    (observed/expected deaths, SMR with 95% CI).
  - `age_stratified_km` and `age_group_cutpoints` for age-stratified
    Kaplan-Meier curves (`ageStratifiedKMPlot`).
  - `adjusted_curves` — covariate-adjusted survival curves
    (`adjustedCurvesPlot`).
  - Interpretation panels: `ageAdjustedInterpretation`,
    `ageAdjustedExplanation`, `ageTimeScaleInterpretation`,
    `ageStandardizationInterpretation`.
- **Weighted log-rank test family** — new `weightedLogRank` option with
  `survivalTestType` choices: `logrank`, `gehan_breslow`, `tarone_ware`,
  `peto_peto`, and `fleming_harrington`; results in
  `weightedLogRankTable` (test, rho, chi-square, df, p-value, weighting)
  plus a `weightedLogRankExplanation` panel. Pairwise group comparisons
  now honor the selected rho weighting.
- **Calibration assessment** — `calibration_curves` with
  `calibration_timepoint` and `calibration_ngroups`; outputs
  `calibrationTable`, per-group `calibrationGroupTable` (predicted
  vs. observed with CIs), `calibrationPlot`, and
  `calibrationInterpretation`.
- **Non-linearity assessment (restricted cubic splines)** —
  `rcs_analysis` with `rcs_variable` and `rcs_knots`; outputs
  `rcsTestTable` (model, df, log-likelihood, AIC, LR chi-square,
  p-value, conclusion), `rcsPlot`, and `rcsInterpretation`.
- **Bootstrap internal validation** — `bootstrapValidation` with
  `bootstrapValN` resamples; `bootstrapValidationTable` reports
  apparent, optimism, and optimism-corrected metrics with a
  `bootstrapValidationExplanation`.
- **REMARK reporting checklist** — `remark_checklist` option renders a
  `remarkChecklist` HTML panel for prognostic-marker reporting.
- **Parametric survival scaffolding** — UI options added
  (`use_parametric`, `parametric_distribution` covering exponential,
  Weibull, log-normal, log-logistic, gamma, generalized gamma, Gompertz,
  and Royston-Parmar spline; `spline_knots`, `spline_scale`,
  `parametric_covariates`, `parametric_extrapolation`,
  `extrapolation_time`, `parametric_diagnostics`,
  `compare_distributions`, `parametric_survival_plots`, `hazard_plots`).
  The parametric backend remains disabled/experimental in this release.

#### Multivariable Survival (`multisurvival`)

- **Cox interaction / effect-modification terms** — new `interactions`
  option (type `Terms`) that crosses variables already chosen as
  explanatory or continuous-explanatory predictors.
  - New `interactionTest` table (interaction HR with 95% CI and p-value)
    and `subgroupHR` table (within-subgroup hazard ratios by moderator
    level).
  - New pure, unit-testable helper module
    `R/multisurvival-interactions.R` (term mapping, formula
    construction, moderator/subgroup summaries), separated from the R6
    backend for maintainability.
  - New `jamovi/js/multisurvival.events.js` model-builder events that
    populate the interaction predictor pool from the
    explanatory/contexpl boxes and prune stale terms.

#### Odds Ratio (`oddsratio`)

- **Firth penalized logistic regression** — new `usePenalized` option
  (via `logistf`) to reduce small-sample bias and handle separation,
  with profile-likelihood confidence intervals and automatic fallback to
  standard logistic regression when `logistf` is unavailable.
- New `predictorLevel` option to set the positive level of the
  predictor.

### Enhanced Existing Modules

#### Multivariable Survival (`multisurvival`)

- Proportional-hazards testing (`ph_cox`) now defaults to **on**,
  surfacing global and per-covariate Schoenfeld residual statistics via
  [`survival::cox.zph`](https://rdrr.io/pkg/survival/man/cox.zph.html)
  (aligned with REMARK reporting guidance).

#### DateTime Converter (`datetimeconverter`)

- Added a unified HTML `notices` panel for validation and conversion
  messaging.

### Notices & Messaging System

- Introduced a four-tier structured HTML notice system (`errors`,
  `strongWarnings`, `warnings`, `infoMessages`) with `.addHtmlMessage()`
  / `.initializeMessageOutputs()` helpers, wired into `survival`,
  `multisurvival`, `oddsratio`, and `outcomeorganizer`. Message outputs
  are reset at the start of each run to prevent accumulation across
  runs.

### Module Removals

- Removed the **Date/DateTime Validator** (`datevalidator`) module
  (backend, header, and all `.a/.r/.u.yaml` files deleted).

### Package Infrastructure

- Raised the minimum jamovi application version (`minApp`) from 1.8.1 to
  **2.7.27**.
- Added internal helper library `R/diagnostichelpers.R` with
  diagnostic-accuracy functions (sensitivity, specificity, PPV/NPV with
  optional prevalence/Bayes adjustment, positive/negative likelihood
  ratios, diagnostic odds ratio, Youden’s J).
- Added internal helper library `R/survivalPower_distributions.R`
  implementing Weibull, log-normal, and piecewise-exponential
  parameterizations and expected-events calculations (Lachin & Foulkes
  reference).
- Expanded dataset roxygen documentation in `R/data.R` (histopathology,
  melanoma, longitudinal, and stage-migration test datasets).
- Reworked reproducible-syntax generation (`asSource`) and hardened
  R-string escaping across module functions. `R/utils.R` gains new
  formula helpers
  ([`.asSurvivalFormula()`](https://www.serdarbalci.com/jsurvival/reference/dot-asSurvivalFormula.md),
  [`.escapeVariableNames()`](https://www.serdarbalci.com/jsurvival/reference/dot-escapeVariableNames.md),
  [`.buildSurvivalFormula()`](https://www.serdarbalci.com/jsurvival/reference/dot-buildSurvivalFormula.md))
  and `%notin%`/`%!in%` operators so that variable names containing
  special characters produce valid Syntax-mode output.
- Hardened error handling in **Time Interval Calculator**
  (`timeinterval`) with additional
  [`jmvcore::reject()`](https://rdrr.io/pkg/jmvcore/man/reject.html)
  guards for invalid or missing dates and unsupported date formats.
- Updated bibliographic references in `jamovi/00refs.yaml`.

------------------------------------------------------------------------

## jsurvival 0.0.32.60 (2025-12-28)

### Major Updates

#### New Modules

- **Date Validator**: Comprehensive date and datetime validation tool.
  - Validates and diagnoses messy date/datetime formats using multiple
    methods (datefixR, anytime, lubridate).
  - Provides detailed audit tables and quality assessment reports.

#### Re-introduced and Enhanced Modules

- **Outcome Organizer**: Re-introduced with significant enhancements.
  - Improved data cleaning and label handling.
  - Contextual validation for different survival analysis types (OS,
    Cause-specific, etc.).
  - Diagnostic messages and glossary for educational support.
- **Time Interval**: Re-introduced as a comprehensive calculator.
  - Robust time interval calculations with landmark analysis support.
  - Person-time calculation for epidemiological studies.
  - Data quality assessment for time intervals.

### Enhancements

#### Survival Analysis

- **Parametric Models**: Improved handling of covariates and formula
  construction.
- **Plots**: Added logic to skip certain plots (e.g., PH assumption,
  residuals) for competing risk analysis to avoid errors.
- **Diagnostics**: Enhanced model summaries and p-value calculations.

#### General

- **Notice System**: Improved notice generation and handling across
  modules.
- **Documentation**: Updated documentation for all modules.

------------------------------------------------------------------------

## jsurvival 0.0.31.84 (2025-10-03)

### New Features

#### Educational Explanations

- Added `showExplanations` option to survival, survivalcont,
  multisurvival, and oddsratio modules
- Built-in HTML explanations for key analysis concepts and statistical
  methods
- Context-sensitive educational content to help users understand their
  analyses

#### Single Arm Survival Enhancements

- **Clinical Analysis Presets**: Pre-configured settings for common
  study types
  - Overall survival analysis (most common)
  - Disease-free survival analysis
  - Treatment effectiveness study
  - Post-surgical outcomes
  - Custom analysis (advanced users)
- **Guided Setup Mode**: Step-by-step guidance for users new to survival
  analysis
- Enhanced person-time calculations and natural language summaries

#### Stage Migration Analysis

- Advanced TNM staging validation and trend analysis
- Improved robustness and explanations for stagemigration module
- Multifactorial analysis capabilities
- Granular table controls and debug outputs

#### Survival Analysis Module

- Enhanced educational explanations for univariate survival
- Improved Cox regression output and interpretation
- Additional visualization options

#### Continuous Survival Analysis

- Educational explanations for cut-point analysis
- Enhanced optimal threshold determination methods
- Improved output tables and visualizations

#### Multivariable Survival

- Educational explanations for multivariable models
- Enhanced model diagnostics and output

#### Odds Ratio Analysis

- Educational explanations for odds ratio interpretation
- Improved forest plots and summary tables

### Module Removals

- Removed `outcomeorganizer` module (functionality integrated into other
  modules)
- Removed `timeinterval` module (functionality integrated into other
  modules)

### Bug Fixes and Improvements

- Enhanced diagnostic test utility functions
- Improved tree analysis for survival models
- Better handling of missing data
- Updated documentation across all modules

### Documentation

- Updated package documentation and vignettes
- Enhanced inline help and tooltips
- Improved example datasets and use cases

------------------------------------------------------------------------

## jsurvival 0.0.3.90 (2024-07-31)

### Major Changes

- Initial implementation of educational explanations system
- Refactored analysis modules for better user experience
- Enhanced natural language summary generation

### New Features

- Stage migration analysis for cancer staging studies
- Advanced survival model options
- Diagnostic test utility functions

------------------------------------------------------------------------

## jsurvival 0.0.3.0 and earlier

For changes in earlier versions, please refer to the git commit history.
