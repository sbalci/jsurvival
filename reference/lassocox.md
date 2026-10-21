# Lasso-Cox Regression

Fits a cross-validated LASSO-penalized Cox model for exploratory model
development. Apparent performance requires internal and external
validation before clinical use.

## Usage

``` r
lassocox(
  data,
  elapsedtime = NULL,
  outcome = NULL,
  outcomeLevel,
  censorLevel,
  explanatory = NULL,
  lambda = "lambda.1se",
  nfolds = 10,
  random_seed = 123456,
  standardize = TRUE,
  suitabilityCheck = TRUE,
  cv_plot = TRUE,
  coef_plot = TRUE,
  survival_plot = TRUE,
  path_plot = FALSE,
  showEncoding = FALSE,
  showReproducibility = FALSE,
  showRCode = FALSE,
  showSummary = FALSE,
  showExplanations = FALSE,
  showMethodologyNotes = FALSE,
  includeClinicalGuidance = FALSE,
  showVariableImportance = FALSE,
  showModelComparison = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- elapsedtime:

  The strictly positive numeric follow-up time until the event or last
  observation. Zero or negative times must be resolved from the time
  origin and measurement resolution; they are not automatically
  adjusted.

- outcome:

  Binary event indicator (event vs censored) with exactly two observed
  values. A numeric 0/1 column is read as 0 = censored and 1 = event;
  for any other coding, such as numeric 1/2 or a factor, `outcomeLevel`
  must name the event value.

- outcomeLevel:

  The level or numeric value of `outcome` that represents the event,
  matched exactly. Required unless `outcome` is numeric 0/1 (read as 1 =
  event). A factor or any other numeric coding without it stops with a
  request to choose it, because level order and numeric size do not say
  which value is the event.

- censorLevel:

  Level of `outcome` considered as censored (no event). If `NULL`, the
  other of the two observed values is used. Together with
  `outcomeLevel`, this must identify the exactly two observed nonmissing
  outcome values. Rows with an actually missing outcome are excluded;
  additional observed event types cause the analysis to stop and must be
  recoded explicitly for a cause-specific analysis.

- explanatory:

  Variables to be considered for selection in the Lasso-Cox regression.
  Time and outcome cannot also be predictors. Infinite values and
  entirely missing predictors are rejected. Constant variables are
  removed before fitting. Categorical predictors are expanded into
  treatment indicator columns, including ordered factors; the first
  observed factor level is the reference. Global/custom contrasts are
  not used. LASSO selects columns individually rather than as a grouped
  factor.

- lambda:

  Cross-validation rule used to select lambda. A valid empty model
  selected by the 1-SE rule is preserved and is not replaced by
  lambda.min.

- nfolds:

  Requested number of cross-validation folds. The count is reduced when
  necessary so every fold contains event and censored observations; at
  least three of each outcome state are required.

- random_seed:

  Random seed for reproducible cross-validation fold assignment.

- standardize:

  Whether glmnet should standardize predictors internally during each
  model fit. Cross-validation training fits therefore do not reuse
  full-cohort means and standard deviations. Displayed coefficients are
  back-transformed by glmnet to the original design-column scale. Factor
  indicators are still penalized and selected separately rather than as
  a grouped factor.

- suitabilityCheck:

  Display an advisory assessment of outcome information, model
  dimension, missingness, collinearity, and an exploratory
  proportional-hazards diagnostic for an unpenalized refit of the
  selected encoded columns.

- cv_plot:

  Whether to show the cross-validation plot.

- coef_plot:

  Whether to show a bar plot of selected-variable coefficients at the
  chosen lambda value.

- survival_plot:

  Whether to show exploratory survival curves after splitting the
  development sample at its median fitted score. The split is not a
  validated clinical cutoff and no group-comparison p-value is reported.

- path_plot:

  Plot coefficients across the fitted log-lambda path with both CV
  choices and counts of nonzero columns. For readability, at most 30
  paths are displayed, ranked by their summed absolute coefficients
  across the path.

- showEncoding:

  Display the original predictor, design column, coding, reference
  level, and selection status.

- showReproducibility:

  Display exact lambda choices, seed, event/censor fold counts, and
  software versions.

- showRCode:

  Generate upstream glmnet R code to refit the same development data
  with the recorded encoding and stratified CV seed. This is not
  external validation.

- showSummary:

  Display a natural-language development summary that labels
  discrimination as apparent and states the required validation.

- showExplanations:

  Display detailed explanations of LASSO Cox regression methodology,
  including regularization concepts and interpretation guidance.

- showMethodologyNotes:

  Show comprehensive technical notes about LASSO regularization,
  cross-validation, and variable selection process.

- includeClinicalGuidance:

  Include guidance for clinical interpretation of LASSO Cox regression
  results, emphasizing that this development analysis alone cannot
  support patient-level decisions.

- showVariableImportance:

  Display absolute penalized coefficients and path-inclusion
  proportions. Path inclusion is not a bootstrap selection frequency or
  stability measure.

- showModelComparison:

  Descriptively compare an unpenalized Cox refit after LASSO selection
  with an unpenalized Cox model using all encoded predictors. All
  statistics are apparent and selection-biased; AIC is not a formal
  comparison here.

## Value

A results object containing:

|                                         |     |     |     |     |           |
|-----------------------------------------|-----|-----|-----|-----|-----------|
| `results$todo`                          |     |     |     |     | a html    |
| `results$suitabilityReport`             |     |     |     |     | a html    |
| `results$modelSummary`                  |     |     |     |     | a table   |
| `results$coefficients`                  |     |     |     |     | a table   |
| `results$performance`                   |     |     |     |     | a table   |
| `results$cv_plot`                       |     |     |     |     | an image  |
| `results$coef_plot`                     |     |     |     |     | an image  |
| `results$survival_plot`                 |     |     |     |     | an image  |
| `results$riskScore`                     |     |     |     |     | an output |
| `results$path_plot`                     |     |     |     |     | an image  |
| `results$encoding`                      |     |     |     |     | a table   |
| `results$reproducibility`               |     |     |     |     | a table   |
| `results$rCode`                         |     |     |     |     | a html    |
| `results$summaryText`                   |     |     |     |     | a html    |
| `results$lassoExplanation`              |     |     |     |     | a html    |
| `results$methodologyNotes`              |     |     |     |     | a html    |
| `results$clinicalGuidance`              |     |     |     |     | a html    |
| `results$variableImportance`            |     |     |     |     | a table   |
| `results$modelComparison`               |     |     |     |     | a table   |
| `results$regularizationPathExplanation` |     |     |     |     | a html    |
| `results$crossValidationExplanation`    |     |     |     |     | a html    |
| `results$riskScoreExplanation`          |     |     |     |     | a html    |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$modelSummary$asDF`

`as.data.frame(results$modelSummary)`

## Examples

``` r
if (FALSE) { # \dontrun{
lassocox(
    data = lassocox_breast_cancer,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    explanatory = c("age", "tumor_size_cm", "grade", "stage",
                    "lymph_nodes_positive", "ki67_percent", "er_status"),
    lambda = "lambda.min")
} # }
```
