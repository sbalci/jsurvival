# Lasso-Cox Regression for Variable Selection in Survival Analysis

## Introduction to Lasso-Cox Regression

### What is Lasso-Cox Regression?

Lasso-Cox regression combines the Cox proportional hazards model with
**L1 regularization** (Lasso penalty) to perform automatic variable
selection in survival analysis. This technique is particularly valuable
when:

- You have many potential predictors (high-dimensional data)
- The number of variables approaches or exceeds the sample size
- You want to identify the most important prognostic factors
- You need to reduce overfitting in survival models

#### Key Advantages

1.  **Automatic Variable Selection**: Shrinks coefficients of less
    important variables to exactly zero
2.  **Handles High-Dimensional Data**: Works even when p \>= n (more
    variables than observations)
3.  **Reduces Overfitting**: Regularization shrinks coefficients, which
    can improve performance in new patients
4.  **Clinical Interpretability**: Produces sparse models with selected
    key variables
5.  **Cross-Validation**: Optimal tuning parameter selection through CV

### When to Use Lasso-Cox Regression

#### Ideal Scenarios

- **Genomic/Biomarker Studies**: Gene expression, proteomics,
  metabolomics data
- **Large Clinical Datasets**: Many potential prognostic factors to
  screen
- **Predictive Modeling**: Building parsimonious prognostic models
- **Exploratory Analysis**: Identifying key variables from large
  candidate sets

#### Prerequisites

- **Survival Outcome**: Time-to-event data with censoring
- **Multiple Predictors**: At least 2 variables (typically many more)
- **Adequate Events**: Enough events to estimate the model; for planning
  a prediction model use a formal sample size calculation (Riley et al.,
  [Stat Med 2019](https://doi.org/10.1002/sim.7992); [BMJ
  2020](https://doi.org/10.1136/bmj.m441)) rather than an
  events-per-variable rule
- **No Perfect Separation**: Variables shouldn’t perfectly predict
  outcomes

## Statistical Background

### The Lasso-Cox Model

The Lasso-Cox regression maximizes the penalized partial log-likelihood
(equivalently, it minimizes
$`-\ell(\beta) + \lambda \sum_{j=1}^p |\beta_j|`$):

``` math
\ell(\beta) - \lambda \sum_{j=1}^p |\beta_j|
```

Where: - $`\ell(\beta)`$ is the Cox partial log-likelihood - $`\lambda`$
is the regularization parameter (tuning parameter) -
$`\sum_{j=1}^p |\beta_j|`$ is the L1 penalty (Lasso)

#### Key Parameters

1.  **Lambda (λ)**: Controls the strength of regularization
    - **lambda.min**: Minimizes cross-validation error
    - **lambda.1se**: Uses 1-standard-error rule (more parsimonious)
2.  **Cross-Validation**: Selects optimal λ through k-fold CV
    - Typically 5-10 folds
    - Balances model fit vs. complexity

### Performance Metrics

#### Concordance Index (C-index)

- Measures discriminative ability
- Range: 0 to 1 among comparable pairs; 0.5 corresponds to chance
  ordering and values below 0.5 indicate reversed ordering. It can be
  undefined when no pairs are comparable.
- Higher values indicate better ordering of event times. There are no
  universal clinical acceptance bands. The displayed C-index is apparent
  performance in the development patients and does not establish
  calibration or usefulness for a clinical decision.

#### Risk Stratification

- **Survival Curves**: Visual assessment of risk group separation

The groups are formed from the same patients used to fit the model, so
their separation is optimistic. The analysis reports no log-rank p-value
or group hazard ratio for this reason.

## Clinical Examples

### Example 1: Lung Cancer Prognostic Model

We’ll analyze a lung cancer dataset to identify key prognostic factors
from clinical variables.

``` r

# For this example, we simulate lung cancer data
library(jsurvival)
set.seed(123)
n <- 180
lung_data <- data.frame(
  # Survival outcome
  follow_up_months = round(rexp(n, rate = 0.05) + runif(n, 0, 12), 1),
  progression = factor(rbinom(n, 1, 0.6), levels = c(0, 1), labels = c("No", "Yes")),
  
  # Clinical variables
  age = round(rnorm(n, 65, 10)),
  gender = factor(sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4))),
  stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE, 
                        prob = c(0.2, 0.3, 0.3, 0.2))),
  histology = factor(sample(c("Adenocarcinoma", "Squamous", "Other"), n, replace = TRUE,
                            prob = c(0.5, 0.3, 0.2))),
  smoking_status = factor(sample(c("Never", "Former", "Current"), n, replace = TRUE,
                                 prob = c(0.2, 0.5, 0.3))),
  tumor_size_cm = round(rnorm(n, 4.5, 2.0), 1),
  ecog_ps = factor(sample(0:2, n, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
  
  # Laboratory values
  hemoglobin = round(rnorm(n, 12.5, 2.0), 1),
  wbc_count = round(rnorm(n, 8.5, 3.0), 1),
  creatinine = round(rnorm(n, 1.1, 0.3), 2)
)

# Display basic characteristics
str(lung_data)
```

#### Running Lasso-Cox Analysis

``` r

# In jamovi: Survival > Penalized Cox Regression > LASSO Cox

# Programmatically (if running in R):
lung_result <- lassocox(
  data = lung_data,
  elapsedtime = "follow_up_months",
  outcome = "progression",
  outcomeLevel = "Yes",
  censorLevel = "No",
  explanatory = c("age", "gender", "stage", "histology", "smoking_status", 
                  "tumor_size_cm", "ecog_ps", "hemoglobin", "wbc_count", "creatinine"),
  lambda = "lambda.1se",  # Use 1-SE rule for more parsimonious model
  nfolds = 10,            # 10-fold cross-validation
  standardize = TRUE      # Standardize variables
)
```

#### Results

These predictors are simulated independently of the outcome. LASSO may
retain no predictors or select noise, depending on the generated sample
and fold allocation. An empty model has an apparent C-index of 0.5; a
nonempty model can look better by chance.

### Example 2: High-Dimensional Genomic Study

This example demonstrates Lasso-Cox with high-dimensional genomic data
where p \>\> n.

``` r

# Simulate high-dimensional genomic data
set.seed(456)
n_patients <- 100
n_genes <- 500

# Create genomic dataset
genomic_data <- data.frame(
  # Survival outcome
  survival_months = round(rweibull(n_patients, shape = 1.5, scale = 24), 1),
  death = factor(rbinom(n_patients, 1, 0.4), levels = c(0, 1), labels = c("No", "Yes")),
  
  # Clinical variables
  age = round(rnorm(n_patients, 60, 12)),
  stage = factor(sample(1:3, n_patients, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
                 levels = 1:3, labels = c("Early", "Intermediate", "Advanced"))
)

# Add gene expression data (standardized)
gene_matrix <- matrix(rnorm(n_patients * n_genes), nrow = n_patients, ncol = n_genes)
colnames(gene_matrix) <- paste0("GENE_", sprintf("%03d", 1:n_genes))

# Combine clinical and genomic data
genomic_data <- cbind(genomic_data, gene_matrix)

cat("Dataset dimensions:", nrow(genomic_data), "patients ×", ncol(genomic_data), "variables")
cat("\nNumber of genes:", n_genes)
cat("\nEvents:", sum(genomic_data$death == "Yes"), "deaths")
```

#### High-Dimensional Analysis Setup

``` r

# For high-dimensional data, consider:
# explanatory reads the names written in the call, so a stored vector of
# names goes through do.call()
genomic_predictors <- c("age", "stage", paste0("GENE_", sprintf("%03d", 1:n_genes)))

genomic_result <- do.call(lassocox, list(
  data = genomic_data,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Yes",
  censorLevel = "No",
  explanatory = genomic_predictors,
  lambda = "lambda.1se",  # Prefer 1-SE rule for high-dimensional data
  nfolds = 5,             # Reduce folds for small samples
  standardize = TRUE      # Essential for genomic data
))
```

#### High-Dimensional Results

The gene columns are pure noise here, so any selected gene is a false
positive. This example shows the shape of the call, not a signature.

### Example 3: Cardiovascular Risk Prediction

Analyzing cardiovascular risk factors. Clinically these predictors are
correlated, but in this simulation each is drawn independently of the
others and of the outcome.

``` r

# Simulate cardiovascular dataset
set.seed(789)
n_subjects <- 200

cvd_data <- data.frame(
  # Outcome
  time_to_event_years = round(rexp(n_subjects, rate = 0.08), 1),
  cv_event = factor(rbinom(n_subjects, 1, 0.3), levels = c(0, 1), 
                    labels = c("No Event", "Event")),
  
  # Demographics
  age = round(rnorm(n_subjects, 62, 15)),
  gender = factor(sample(c("Male", "Female"), n_subjects, replace = TRUE)),
  
  # Clinical measurements (correlated variables)
  bmi = round(rnorm(n_subjects, 28, 6), 1),
  systolic_bp = round(rnorm(n_subjects, 135, 25)),
  diastolic_bp = round(rnorm(n_subjects, 82, 15)),
  total_cholesterol = round(rnorm(n_subjects, 210, 40)),
  hdl_cholesterol = round(rnorm(n_subjects, 45, 12)),
  ldl_cholesterol = round(rnorm(n_subjects, 125, 35)),
  
  # Medical history
  diabetes = factor(sample(c("No", "Yes"), n_subjects, replace = TRUE, prob = c(0.7, 0.3))),
  hypertension = factor(sample(c("No", "Yes"), n_subjects, replace = TRUE, prob = c(0.5, 0.5))),
  smoking = factor(sample(c("Never", "Former", "Current"), n_subjects, replace = TRUE,
                          prob = c(0.5, 0.3, 0.2))),
  
  # Medications
  statin_use = factor(sample(c("No", "Yes"), n_subjects, replace = TRUE, prob = c(0.4, 0.6)))
)

str(cvd_data)
```

#### Cardiovascular Analysis

``` r

cvd_result <- lassocox(
  data = cvd_data,
  elapsedtime = "time_to_event_years",
  outcome = "cv_event",
  outcomeLevel = "Event",
  censorLevel = "No Event",
  explanatory = c("age", "gender", "bmi", "systolic_bp", "diastolic_bp",
                  "total_cholesterol", "hdl_cholesterol", "ldl_cholesterol",
                  "diabetes", "hypertension", "smoking", "statin_use"),
  lambda = "lambda.min",
  nfolds = 10
)
```

## Interpreting Results

### Understanding the Output Tables

#### 1. Model Summary Table

- **Encoded Predictor Columns**: Number of candidate columns after
  factors are expanded
- **Selected Predictor Columns**: Columns with non-zero coefficients
- **Column Selection Proportion**: Percentage of columns retained
- **Optimal Lambda** and **Penalty Selected By**: The selected penalty
  and the rule used
- **Sample Size**, **Number of Events** and **Censoring Rate**: Data
  characteristics
- **Original Candidate Predictors** and **Selected Original
  Predictors**: The same counts before encoding

#### 2. Penalized Coefficients Table

- **Variable**: Selected predictor names
- **Penalized Coefficient**: Shrunken log hazard ratio (β), without a
  confidence interval
- **Penalized Hazard Ratio**: exp(β), shrunken and without a confidence
  interval
- **Scale-Adjusted Magnitude \|beta\| x SD**: Absolute change in the
  linear predictor per 1 SD of that column. This is descriptive, not a
  causal importance.

#### 3. Apparent Development Performance Table

- **Apparent C-index**: Discrimination in the same patients used to fit
  the model, which is optimistic

### Clinical Interpretation Guidelines

#### Coefficient Interpretation

A penalized coefficient is shrunken toward zero and depends on the unit
of the predictor, so do not grade it as a strong or weak effect. A
positive coefficient means a higher hazard with higher values of that
column, and a negative coefficient a lower hazard. These are
associations, not causal effects.

#### C-index Interpretation

A C-index describes ordering among comparable patient pairs, with tied
predictions receiving half credit. Values range from 0 to 1 when
comparable pairs exist; 0.5 is chance ordering and values below 0.5
indicate reversed ordering. It is undefined when no pairs are
comparable. There are no universal cutoffs for clinical usefulness.
Interpret discrimination alongside its uncertainty, case mix, censoring,
calibration and the decisions the model would inform. The displayed
development-sample estimate is apparent performance; lambda
cross-validation does not validate that estimate. See the [survival
concordance
reference](https://stat.ethz.ch/R-manual/R-devel/library/survival/html/concordance.html).

### Visualization Interpretation

#### 1. Cross-Validation Plot

- **X-axis**: Log lambda values
- **Y-axis**: Cross-validation error
- **Vertical Lines**:
  - Blue (lambda.min): Minimum CV error
  - Green (lambda.1se): 1-SE rule (more parsimonious)

#### 2. Coefficient Plot

- **Red bars**: Higher hazard (positive coefficients)
- **Blue bars**: Lower hazard (negative coefficients)
- **Bar length**: Penalized coefficient on its original predictor scale;
  lengths cannot rank clinical importance across different units.

#### 3. Survival Curves

- **Separation**: Describes the development patients after fitting and
  grouping on those same patients; separation is optimistic and does not
  establish predictive performance in new patients.
- **Risk tables**: Numbers at risk over time

#### Cardiovascular Results

With `lambda.min`, a weaker penalty than `lambda.1se`, this run still
selects no predictor and reports an apparent C-index of 0.5, as expected
for noise. A different seed could select a few, and they would be false
positives.

## Clinical Applications

### 1. Biomarker Discovery

#### Genomic Profiling

The call is the one in Example 2: `lambda.1se`, with every gene as a
candidate predictor.

Any signature found this way is a candidate. It needs validation in
independent patients before it means anything clinically.

### 2. Clinical Risk Prediction

#### Multi-variable Risk Models

``` r

# Clinical risk model development on the bundled lassocox_breast_cancer data
data("lassocox_breast_cancer", package = "jsurvival")

risk_model <- lassocox(
  data = lassocox_breast_cancer,
  elapsedtime = "survival_months",
  outcome = "death",
  outcomeLevel = "Dead",
  censorLevel = "Alive",
  explanatory = c("age", "tumor_size_cm", "grade", "stage", "lymph_nodes_positive",
                  "er_status", "pr_status", "her2_status", "ki67_percent", "lvi"),
  lambda = "lambda.min",            # Retain more clinical variables
  nfolds = 10
)
```

This run excludes 5 of 250 rows for missing data and selects one column,
`ki67_percent`, with a penalized hazard ratio of 1.01 per percentage
point and an apparent C-index of 0.62. Both figures are optimistic until
validated, and the shrunken hazard ratio has no confidence interval.

### 3. Treatment Effect

A main-effects LASSO Cox model selects prognostic variables. It does not
show which patients benefit from a treatment, because that requires a
treatment-by-marker interaction, preferably in a randomized trial.

## Advanced Topics

### Model Validation

#### Cross-Validation Strategy

1.  **Tuning**: Built-in cross-validation selects λ; the displayed
    C-index is still apparent performance
2.  **External Validation**: Test on independent dataset
3.  **Bootstrap Validation**: Assess model stability
4.  **Temporal Validation**: Validate on more recent data

#### Validation Designs

- **Split-sample**: Fit on one part of the data and test on the rest.
  This uses the data less efficiently than the options below.
- **Time-based**: Fit on patients enrolled earlier and test on those
  enrolled later.
- **Multi-center**: Fit in some centers and test in another.

These checks are done outside this analysis. Apply the frozen
coefficients to the new patients; do not refit the model there.

### Dealing with Missing Data

#### Preprocessing Strategies

1.  **Complete Case Analysis**: Default approach
2.  **Multiple Imputation**: For missing at random data
3.  **Sensitivity Analysis**: Missing-not-at-random mechanisms need
    explicit assumptions and sensitivity analyses; a missingness
    indicator alone does not resolve the bias
4.  **Domain Knowledge**: Prespecify imputation models with clinical and
    statistical input

The analysis drops rows with a missing value in any selected variable,
and **Rows Excluded (Missing Data)** in the Model Summary reports how
many. Multiple imputation is done with other tools before this analysis.

For performance assessment, fit learned imputation and other learned
preprocessing within each training resample. jsurvival does not pool
LASSO fits across multiply imputed datasets.

### Feature Engineering

#### Variable Transformation

Log-transform a skewed biomarker before adding it as a candidate. Add
polynomial or interaction columns only when they are planned in advance.
Avoid binning a continuous predictor, because it discards information.

## Troubleshooting Common Issues

### 1. No Variables Selected

**Problem**: Lasso selects zero variables (λ too large)

**Solutions**:

- Check whether this is the right answer: with no real signal, selecting
  nothing is correct.
- Set **Lambda selection rule** to **Minimum CV error** instead of
  **1-SE rule**.
- Review predictors that are highly correlated with each other.

### 2. Too Many Variables Selected

**Problem**: Model selects too many variables (overfitting)

**Solutions**:

- Set **Lambda selection rule** to **1-SE rule** for more
  regularization.

### 3. Poor Model Performance

**Problem**: Low C-index or poor risk stratification

**Solutions**:

- Check data quality, including the follow-up times and the event
  coding.
- Check that there are enough events; **Number of Events** is in the
  Model Summary.
- Consider whether the candidate predictors carry prognostic information
  at all. A low C-index may be the correct result.

### 4. Convergence Issues

**Problem**: Algorithm doesn’t converge

**Solutions**:

- Keep **Internal standardization** ticked.
- Investigate extreme values; correct errors, and do not delete a value
  only because it is extreme.
- Check for a predictor that almost perfectly separates events from
  non-events.

With few events per column, the penalty search can stop early. The
analysis then warns that it stopped after some of the lambda values,
because smaller penalties gave a patient a linear predictor above 30,
where glmnet 5.0 cannot compute the cross-validation curve correctly.
The **Lambda values used / searched** row in **Reproducibility details**
shows how many were kept.

## Best Practices

### Study Design Considerations

#### Sample Size Planning

1.  **Formal Calculation**: Use a sample size calculation for prediction
    models (Riley et al., [Stat Med
    2019](https://doi.org/10.1002/sim.7992); [BMJ
    2020](https://doi.org/10.1136/bmj.m441)) rather than an
    events-per-variable rule
2.  **Total Sample Size**: Consider expected censoring rate
3.  **Effect Size**: Larger effects require smaller samples
4.  **Multiple Testing**: Account for variable selection process

#### Variable Selection

Choose candidates on clinical grounds, before looking at the outcome. Do
not pre-filter by univariable p-values: that happens outside the
cross-validation, defeats the penalty and biases selection. Among highly
correlated predictors, LASSO picks one almost arbitrarily, so do not
read a dropped predictor as unimportant.

### Reporting Standards

#### Essential Elements for Publication

1.  **Methods Section**:
    - Cross-validation procedure (k-folds)
    - Lambda selection method (min vs. 1se)
    - Standardization approach
    - Missing data handling
2.  **Results Section**:
    - Number of variables considered and selected
    - Cross-validation performance
    - Final model coefficients and hazard ratios
    - Discrimination metrics (C-index)
    - Risk stratification results
3.  **Figures and Tables**:
    - Cross-validation plot
    - Coefficient plot
    - Survival curves by risk groups
    - Model performance summary

#### Reproducibility Checklist

- Record the **Random seed**, which fixes the cross-validation folds.
- Tick **Reproducibility details** for the lambda values, software
  versions and fold counts.
- Tick **Reproducible R code** to keep the code for the fit.
- Keep a dictionary of variable definitions, units and level coding.

## Conclusion

Lasso-Cox regression is a method for variable selection in survival
analysis, used for:

1.  **High-dimensional data** where traditional methods fail
2.  **Biomarker discovery** in genomic and proteomic studies
3.  **Clinical risk modeling** with many potential predictors
4.  **Exploratory analysis** to identify key prognostic factors

The `lassocox` function in jsurvival fits this model from jamovi or R.

### Key Takeaways

- **Choose appropriate λ**: lambda.1se for sparsity, lambda.min for
  performance
- **Validate externally**: Internal CV is not sufficient for clinical
  use
- **Consider clinical context**: Statistical significance ≠ clinical
  relevance
- **Report transparently**: Include all analysis decisions and
  limitations

------------------------------------------------------------------------

*For more information about jsurvival, visit the [jsurvival
website](https://www.serdarbalci.com/jsurvival/).*
