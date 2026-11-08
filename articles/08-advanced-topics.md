# Advanced Survival Analysis with jSurvival

## Overview

This vignette covers advanced survival analysis techniques and
methodological considerations when using jSurvival. It addresses complex
scenarios that researchers commonly encounter in clinical and
epidemiological studies.

The code below uses simulated data, created here so that each call runs
as written. The data carry no real effect.

``` r

library(jsurvival)
set.seed(1)
n <- 300
mydata <- data.frame(
  time_months = rexp(n, rate = 1 / 36),
  death = rbinom(n, 1, 0.6),
  treatment = sample(c("Control", "Experimental"), n, replace = TRUE),
  sex = sample(c("Female", "Male"), n, replace = TRUE),
  age = round(rnorm(n, 62, 10)),
  center = sample(c("Center A", "Center B", "Center C"), n, replace = TRUE),
  response_status = sample(c("Non-responder", "Responder"), n, replace = TRUE),
  biomarker_value = rlnorm(n, meanlog = 2, sdlog = 0.6)
)
biomarker_data <- mydata
original_data <- mydata

cause <- sample(c("Alive", "Cancer", "Other"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
competing_data <- mydata
competing_data$death_cancer <- as.numeric(cause == "Cancer")
competing_data$death_other <- as.numeric(cause == "Other")

study1_data <- mydata[1:100, ]
study2_data <- mydata[101:200, ]
study3_data <- mydata[201:300, ]

data("histopathology", package = "jsurvival")
```

## Advanced Cox Regression Topics

### Checking Proportional Hazards Assumption

The Cox proportional hazards model assumes that hazard ratios remain
constant over time. Violation of this assumption can lead to misleading
results.

#### Visual Assessment

``` r

# ph_cox reports the proportional hazards test and draws the Schoenfeld plot
ph_check <- survival(
  data = mydata,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = treatment,
  ph_cox = TRUE,
  endplot = 120   # Extended follow-up for visual inspection
)
```

**Visual Signs of PH Violation**: - Crossing survival curves - Changing
hazard ratios over time - Non-random patterns in Schoenfeld residuals

#### Solutions for PH Violations

##### Option 1: Stratified Cox Model

``` r

# When PH assumption is violated for a categorical variable
# Use stratification approach in multivariable analysis
stratified_result <- multisurvival(
  data = mydata,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = c("treatment", "sex"),
  contexpl = "age",
  use_stratify = TRUE,
  stratvar = "center"
)
```

##### Option 2: Time-Dependent Effects

For variables with time-varying effects, consider: - Landmark analyses
at multiple time points - Piecewise exponential models - Flexible
parametric models

### Model Selection and Variable Selection

Choose covariates in advance from clinical knowledge. Screening by
univariable p-value biases estimates; use `lassocox` if you need
data-driven selection, and validate it.

#### Model Validation

External validation applies the frozen coefficients to new data and
assesses discrimination and calibration. jsurvival does not do this; use
`Add Risk Score to Data` only on the development cohort.

### Handling Missing Data

#### Complete Case Analysis

``` r

# Default approach - uses only complete cases
complete_result <- survival(
  data = mydata,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = treatment
)
```

#### Multiple Imputation Approach

jsurvival has no imputation. Pool on the log hazard ratio with Rubin’s
rules using `mice` and `coxph` directly.

## Cut-point Optimization

### Methodological Considerations

#### Multiple Cut-point Testing

There is **no p-value adjustment option** on `survivalcont`, and passing
`padjustmethod` raises `unused argument`, because the adjustment for a
single cut-off is built in. With `findcut = TRUE` the cut-off table
reports an **Adjusted p**, a minimum-p correction for the search over
all candidate cut-offs; the note under the table names the
approximations used. It is conservative. A p-value computed afterwards
on the low and high groups ignores the search and is not a valid test.

With several cut-offs chosen from the outcome (`cutoff_method` of
`"recursive"`, `"tree"` or `"minpval"`) no adjusted p-value exists, so
the log-rank p-value is labeled unadjusted and is not printed on the
plot. Any data-derived cut-off needs validation in independent data.

The analysis also offers control over how the cut-offs themselves are
derived, via `multiple_cutoffs`, `num_cutoffs` and `cutoff_method`:

``` r

cutpoint_analysis <- survivalcont(
  data = biomarker_data,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  contexpl = biomarker_value,
  findcut = TRUE,           # single cut-off, maximally selected rank statistic
  multiple_cutoffs = TRUE,  # or several candidate cut-offs
  num_cutoffs = "two",      # "two" (3 groups), "three" (4), "four" (5)
  cutoff_method = "quantile" # "quantile", "recursive", "tree", "minpval"
)
```

#### Split-Sample Check of Cut-points

``` r

# Split data into training and validation sets
set.seed(123)
train_idx <- sample(nrow(biomarker_data), nrow(biomarker_data) * 0.7)
train_data <- biomarker_data[train_idx, ]
test_data <- biomarker_data[-train_idx, ]

# Find cut-point in training data
train_cutpoint <- survivalcont(
  data = train_data,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  contexpl = biomarker_value,
  findcut = TRUE
)

# Apply cut-point to test data
optimal_cutpoint <- train_cutpoint$rescutTable$asDF$cutpoint[1]
test_data$biomarker_high <- ifelse(test_data$biomarker_value > optimal_cutpoint,
                                   "High", "Low")

validation_result <- survival(
  data = test_data,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = biomarker_high
)
```

The fitted cutoff convention is **Low ≤ cutoff; High \> cutoff**. Freeze
the exact cutoff and its equality rule before testing held-out patients.
A random split from one source is internal validation, not external
validation. For small cohorts it is inefficient; use resampling that
repeats the entire selection process when assessing optimism.

### Alternative Approaches to Dichotomization

#### Tertiles or Quartiles

``` r

# Instead of single cut-point, use tertiles
biomarker_data$biomarker_tertile <- cut(biomarker_data$biomarker_value,
                                        breaks = quantile(biomarker_data$biomarker_value,
                                                         c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                        labels = c("Low", "Medium", "High"),
                                        include.lowest = TRUE)

tertile_analysis <- survival(
  data = biomarker_data,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = biomarker_tertile,
  analysistype = "overall"
)
```

#### Spline Analysis

Restricted cubic splines are built in:
[`survival()`](https://www.serdarbalci.com/jsurvival/reference/survival.md)
gained `rcs_analysis` for this, so there is no need to leave jsurvival
to avoid dichotomizing a continuous predictor:

``` r

spline_analysis <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  rcs_analysis = TRUE,
  rcs_variable = "Age",   # the continuous predictor to model flexibly
  rcs_knots    = 4        # 3 knots for a modest cohort, 5 when data allow
)
```

This tests whether the log-hazard really is linear in the predictor. If
it is not, a dichotomization chosen from the same data will look better
than it is. See [Features Added Since the Earlier
Releases](#features-added-since-the-earlier-releases) for the
calibration and internal-validation options that pair with it.

## Time-Dependent Covariates

### Landmark Analysis Implementation

#### Multiple Landmark Points

``` r

# Analyze survival at different landmark times. jsurvival applies the landmark itself,
# so pass the original time column. Do not shift the time as well, or it is shifted
# twice. Patients whose time equals the landmark exactly are excluded.
landmark_times <- c(6, 12, 24)  # months
landmark_results <- list()

for(t in landmark_times) {
  landmark_results[[paste0("Month_", t)]] <- survival(
    data = original_data,
    elapsedtime = time_months,
    outcome = death,
    outcomeLevel = "1",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = response_status,
    uselandmark = TRUE,
    landmark = t
  )
}
```

#### Dynamic Prediction

``` r

# Conditional survival probabilities
# Probability of surviving additional 2 years given survival to 1 year

conditional_survival <- singlearm(
  data = mydata,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  uselandmark = TRUE,
  landmark = 12,
  cutp = "24",  # Additional 2 years
  timetypeoutput = "months"
)
```

## Competing Risks Analysis

### Cause-Specific Hazards

``` r

# Separate analyses for different causes of death
# Cancer-specific mortality
cancer_death <- survival(
  data = competing_data,
  elapsedtime = time_months,
  outcome = death_cancer,  # 1 = cancer death, 0 = alive or other death
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = treatment,
  ph_cox = TRUE
)

# Other-cause mortality
other_death <- survival(
  data = competing_data,
  elapsedtime = time_months,
  outcome = death_other,   # 1 = other death, 0 = alive or cancer death
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = treatment,
  ph_cox = TRUE
)
```

### Subdistribution Hazards (Fine-Gray Model)

The Fine-Gray model keeps patients who had a competing event in the risk
set, with decreasing weights. In Multivariable Survival, use **Multiple
event levels** with **Survival Type** set to **Competing Risk**. Its
subdistribution hazard ratios describe cumulative incidence, not the
cause-specific rate.

## Sample Size and Power Calculations

### Required Sample Size

``` r

# For planning future studies
target_hr <- 0.70        # Clinically meaningful difference
alpha <- 0.05            # Type I error
power <- 0.80            # Desired power
accrual_time <- 24       # months
followup_time <- 36      # months
median_survival <- 30    # months in control group

# Use specialized software or formulas for calculation
# Required events ≈ 4 * (Z_α/2 + Z_β)² / (log(HR))²
```

## Meta-Analysis of Survival Data

### Individual Patient Data Meta-Analysis

``` r

# Combine multiple datasets
combined_data <- rbind(
  transform(study1_data, study = "Study1"),
  transform(study2_data, study = "Study2"),
  transform(study3_data, study = "Study3")
)

# Stratified analysis by study
meta_result <- multisurvival(
  data = combined_data,
  elapsedtime = time_months,
  outcome = death,
  outcomeLevel = "1",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  explanatory = c("treatment", "sex"),
  contexpl = "age",
  use_stratify = TRUE,
  stratvar = "study"
)
```

### Fixed Effects vs. Random Effects

A pooled
[`survival()`](https://www.serdarbalci.com/jsurvival/reference/survival.md)
call that ignores study is not a fixed-effects meta-analysis. Stratify
by study, as above, for a one-stage analysis. Random-effects models need
specialized meta-analysis functions outside jsurvival.

## Quality Control and Validation

### Data Quality Checks

``` r

# Check for data inconsistencies
max_followup <- 200  # months; set this to the longest possible follow-up in your study
data_checks <- list(
  # Negative survival times
  negative_times = sum(mydata$time_months < 0, na.rm = TRUE),
  
  # Events after last follow-up
  impossible_events = sum(mydata$death == 1 & mydata$time_months > max_followup),
  
  # Missing key variables
  missing_time = sum(is.na(mydata$time_months)),
  missing_event = sum(is.na(mydata$death)),
  
  # Extreme values
  extreme_times = sum(mydata$time_months > 200, na.rm = TRUE)  # >16 years
)

print(data_checks)
```

### Model Diagnostics

``` r

# Concordance index (C-index)
# Measures discriminative ability of the model
# No universal C-index threshold establishes clinical usefulness.
# Evaluate discrimination, uncertainty, calibration and decision consequences.
# The apparent value from the model-development patients is optimistic.

# Calibration assessment
# Agreement between predicted and observed survival

# Model comparison using AIC/BIC
# Lower values indicate better fit
```

## Reporting and Interpretation

### Effect Size Interpretation

``` r

# Hazard Ratio Interpretation:
# HR = 0.50: 50% lower fitted hazard relative to the reference
# HR = 0.75: 25% lower fitted hazard relative to the reference
# HR = 0.90: 10% lower fitted hazard relative to the reference
# HR = 1.00: Equal fitted hazards
# HR = 1.25: 25% higher fitted hazard relative to the reference
# HR = 2.00: 100% higher fitted hazard relative to the reference
# These are relative associations. Clinical importance depends on baseline
# risk, time horizon, uncertainty, adverse outcomes and study design.

# Hypothetical causal survival contrast at the same five-year horizon:
survival_control <- 0.60
survival_treatment <- 0.70
absolute_benefit <- survival_treatment - survival_control
nnt <- 1 / absolute_benefit  # 10 at five years, for these illustrative values
# A treatment NNT needs a defensible causal comparison and uncertainty.
# Do not infer it from an observational HR or from these invented values.
```

### Confidence Interval Interpretation

``` r

# HR = 0.75 (95% CI: 0.60-0.95)
# Interpretation:
# - Point estimate is a 25% lower fitted hazard relative to the reference
# - HR interval endpoints correspond to 5% to 40% lower relative hazard
# - These endpoints are not bounds on absolute benefit or all plausible effects
# - The interval excludes 1 for the stated model and confidence procedure
# - Repeated use of that procedure covers the true parameter in 95% of studies,
#   under its assumptions; it assigns no probability to this fixed interval.
```

## Conclusion

Advanced survival analysis requires careful consideration of:

1.  **Model assumptions** and their validation
2.  **Missing data patterns** and appropriate handling
3.  **Multiple comparisons** and adjustment strategies
4.  **Clinical relevance** beyond statistical significance
5.  **Validation** in independent datasets

Key principles for advanced analysis:

- **Pre-specify analysis plans** to avoid data dredging
- **Validate findings** in independent cohorts when possible
- **Consider clinical context** in statistical decisions
- **Report limitations** and assumptions clearly
- **Collaborate with statisticians** for complex analyses

## Features Added Since the Earlier Releases

These options are all live in
[`survival()`](https://www.serdarbalci.com/jsurvival/reference/survival.md)
but were not covered anywhere in this guide until now. Every call below
was run against the bundled `histopathology` data before being written
down.

### Weighted Log-Rank Tests

The standard log-rank test gives each event time weight 1 and is well
suited to proportional-hazards alternatives. `weightedLogRank = TRUE`
also reports the
[`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)
G-rho tests, which weight event times by pooled survival raised to rho.
Positive rho emphasizes earlier events relative to later events; it does
not guarantee greater power for every early difference. These options do
not implement the full two-parameter late-effect or MaxCombo family. See
the [survdiff
reference](https://stat.ethz.ch/R-manual/R-patched/library/survival/html/survdiff.html).

``` r

res <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  weightedLogRank  = TRUE,
  survivalTestType = "fh_rho1"   # "logrank", "fh_rho0_5", "fh_rho1"
)
res$weightedLogRankTable$asDF
#>                                   test rho    chisq df   pvalue
#> 1                  Log-Rank (standard) 0.0 0.220312  1 0.638802
#> 2         Fleming-Harrington (rho=0.5) 0.5 0.128574  1 0.719915
#> 3 Peto-Peto (Fleming-Harrington rho=1) 1.0 0.066619  1 0.796325
```

All three tests are reported whichever `survivalTestType` you pick, each
with the weighting it applies: `rho = 0` weights all time points
equally, `rho = 0.5` gives moderate emphasis to early differences, and
`rho = 1` weights by S(t) and emphasizes early events. Choose the test
**before** looking at the results. Picking the smallest p-value from
three tests is not a test.

### Age-Adjusted Analysis

Age can confound a group comparison when it is associated with both
group and outcome. Select adjustment variables from the study question
and design; age is not automatically the dominant predictor. The age
options include the following:

``` r

res_age <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  age_adjustment = TRUE,
  age_variable   = "Age"
)
res_age$ageAdjustedCoxTable$asDF
#>   variable            levels   n  hr_unadjusted hr_age_adjusted
#> 1    group         Treatment 245 0.94 (0.69-1.27) 0.94 (0.69-1.28)
#> 2      Age per unit (p=0.11) <NA>                - 0.99 (0.98-1.00)
```

The table puts the unadjusted and age-adjusted hazard ratios side by
side, which is the comparison that actually answers “is this confounded
by age?”. The remaining options address different questions and can be
combined with the above:

| Option | Question it answers |
|----|----|
| `age_interaction = TRUE` | Does the treatment effect differ by age? |
| `age_stratified_cox = TRUE` | Adjust for age without assuming proportional hazards across age |
| `age_group_cutpoints` | Age bands for stratification (default `"50, 65, 75"`) |
| `age_time_scale = TRUE` | Use age, not time-on-study, as the time scale |
| `age_standardization = TRUE` | Standardized mortality ratio; `age_standardization_method` is `"indirect"` or `"direct"` |
| `age_stratified_km = TRUE` | Kaplan-Meier curves per age band |

### Parametric Survival Models

Kaplan-Meier and Cox are non-parametric and semi-parametric
respectively; neither extrapolates beyond the observed follow-up. When
you need to, `use_parametric = TRUE` fits a parametric model.

``` r

res_par <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  use_parametric          = TRUE,
  parametric_distribution = "weibull",   # exp, weibull, lnorm, llogis, gamma,
                                         # gengamma, gompertz, survspline
  parametric_covariates   = TRUE,
  compare_distributions   = TRUE         # fit several and compare by information criterion
)
```

With `parametric_distribution = "survspline"`, `spline_knots` (default
3) and `spline_scale` (`"hazard"`, `"odds"` or `"normal"`) control the
flexible-parametric fit. Extrapolation beyond the follow-up window is a
modeling assumption, not a measurement. Say so when you report it.

### Calibration, Non-Linearity and Internal Validation

``` r

res_val <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  calibration_curves    = TRUE,   # predicted vs observed survival
  calibration_timepoint = 12,     # in the analysis time unit
  calibration_ngroups   = 5,      # risk groups on the calibration plot
  rcs_analysis          = TRUE,   # restricted cubic splines: is the effect linear?
  rcs_variable          = "Age",
  rcs_knots             = 4,
  bootstrapValidation   = TRUE,   # bootstrap internal validation of the C-index
  bootstrapValN         = 200     # bootstrap resamples
)
```

`rcs_analysis` answers a question that is easy to skip: a continuous
predictor entered linearly assumes the log-hazard is linear in it, and
that is frequently false for age and for biomarkers. Bootstrap
validation here is *internal*: it corrects for optimism and does not
substitute for an independent cohort.

### Model Performance in `multisurvival`

Four
[`multisurvival()`](https://www.serdarbalci.com/jsurvival/reference/multisurvival.md)
options address the question a Cox table cannot answer on its own: how
well does this model predict? All four ran against the bundled
`histopathology` data.

``` r

res_ms <- multisurvival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = c("Group", "LVI"), contexpl = "Age",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  ci_optimism      = TRUE,   # optimism-corrected C-index by bootstrap
  ci_optimism_boot = 200,    # resamples (default 150)
  compare_models   = TRUE,   # covariate contribution by single-term deletion
  show_survmetrics = TRUE,   # Brier score and time-dependent AUC
  survmetrics_timepoints = "12, 36, 60",
  survmetrics_show_plots = TRUE,
  ac = TRUE,                 # adjusted probability curve
  adjexplanatory = "Group",
  ac_summary = TRUE          # adjusted probability summary tables
)
```

| Option | What it gives you |
|----|----|
| `ci_optimism` | The apparent C-index of a model fitted and evaluated on the same rows is optimistic. This bootstraps the whole fitting procedure to estimate that optimism and subtracts it. Report the corrected figure. |
| `compare_models` | Drops each term in turn and reports the change in fit, which answers “what is this covariate contributing?” more honestly than reading its p-value. |
| `show_survmetrics` | Brier score (calibration and discrimination together, lower is better) and time-dependent AUC at the timepoints you name in `survmetrics_timepoints`. |
| `ac_summary` | With `ac = TRUE` and `adjexplanatory` set, adds adjusted probability summary tables to the adjusted survival curves. |

Optimism correction is still *internal* validation. It tells you how
much this modeling procedure overfits these data; it cannot tell you how
the model behaves in another hospital’s cohort.

### Reporting Aids

``` r

res_report <- survival(
  data = histopathology,
  elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
  explanatory = "Group",
  dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
  adjusted_curves  = TRUE,   # covariate-adjusted survival curves
  remark_checklist = TRUE,   # REMARK reporting checklist for prognostic marker studies
  showSummaries    = TRUE    # natural-language summaries alongside the tables
)
```

`remark_checklist` is worth running on any prognostic-marker analysis
you intend to publish; it lists the REMARK items and is quicker than
reconstructing them from the paper afterwards.

For complex analyses beyond the scope of basic jSurvival functions,
consider: - Specialized R packages (survival, survminer, rms, cmprsk) -
Statistical software with advanced survival capabilities - Consultation
with biostatisticians

Additional resources: - [Survival Analysis
Handbook](https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html) -
[Clinical Prediction
Models](https://www.clinicalpredictionmodels.org/) - [STROBE
Guidelines](https://www.strobe-statement.org/) for observational studies
