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
  a grouped factor. When this is off, selection depends on the units of
  each predictor; changing a unit can change which predictors are
  selected.

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
  paths are displayed: selected columns first, then by summed absolute
  coefficient times column SD across the path.

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
| `results$suitabilityChecks`             |     |     |     |     | a table   |
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

`results$suitabilityChecks$asDF`

`as.data.frame(results$suitabilityChecks)`

## Examples

``` r
# \donttest{
lassocox(
    data = lassocox_breast_cancer,
    elapsedtime = "survival_months",
    outcome = "death",
    outcomeLevel = "Dead",
    censorLevel = "Alive",
    explanatory = c("age", "tumor_size_cm", "grade", "stage",
                    "lymph_nodes_positive", "ki67_percent", "er_status"),
    lambda = "lambda.min")
#> 
#>  LASSO-COX REGRESSION
#> 
#>  <div class='alert alert-warning'>
#> 
#>  Analysis Notes
#> 
#>  Excluded 5 row(s) with missing values in time/outcome/predictors
#>  (complete-case analysis).Categorical predictors are represented by
#>  indicator columns and LASSO selects those columns individually rather
#>  than selecting each factor as a group. Factor(s): grade, stage,
#>  er_status.
#> 
#>  Suitability Checks                                                                                                                                                                                                                                                                                                                                                                                                   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Status               Check                   Value                                                        Detail                                                                                                                                                                                                                                                                                                   
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Caution              Events-Per-Variable     9.4 (nEvents=85, p=9)                                        Limited event information per encoded candidate column. Penalization enables fitting but does not make selection or performance reliable.                                                                                                                                                                
#>    No issue detected    Regularization Need     p=9, n=245                                                   Penalized regression is a reasonable candidate approach for this data dimension; this check does not establish sample-size adequacy.                                                                                                                                                                     
#>    Not determined       Sample Size             n=245, events=85, p=9                                        Sample-size adequacy cannot be determined from total n or a universal EPV cutoff. Use a prediction-model sample-size calculation based on outcome frequency, candidate parameters, anticipated model fit, and target optimism.                                                                           
#>    No issue detected    Event Rate              34.7% (85/245)                                               Event and censoring observations are both represented; adequacy still depends on absolute counts and model complexity.                                                                                                                                                                                   
#>    Caution              Multicollinearity       Max |r| = 0.75                                               Moderate collinearity. LASSO may select one of several correlated columns, and that choice can be unstable. Consider Elastic Net if retaining correlated predictors is important. Top correlated encoded-column pairs: gradeGrade 3 & ki67_percent (r=0.75); stageIII & lymph_nodes_positive (r=0.71).   
#>    Caution              Data Quality            5 rows excluded; 0 constant predictors or columns removed    Complete-case analysis excluded 5 rows (2.0%). Even a small excluded fraction can bias results when missingness is informative.                                                                                                                                                                          
#>    No issue detected    Proportional Hazards    Selected-refit test p = 0.172                                No evidence of non-proportional hazards in an exploratory unpenalized refit of the selected encoded columns. Limited power and post-selection use mean that this does not prove the assumption.                                                                                                          
#>  ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#> 
#> 
#>  <div style='background-color: rgba(255, 202, 33, 0.23); color:
#>  inherit; border: 1px solid #ffeeba; padding: 12px;'>Overall: Data is
#>  usable but review the flagged items.<div style='margin-top: 12px;
#>  padding: 10px; background-color: rgba(138, 155, 172, 0.06);
#>  border-radius: 4px; color: inherit;'>Recommendations:<ul
#>  style='margin: 6px 0;'>Consider an analysis that supports grouped or
#>  elastic-net penalties when correlated predictors should be retained
#>  together; ordinary LASSO may select only one member of a correlated
#>  set.If using multiple imputation, repeat imputation and all
#>  preprocessing inside each resampling training split; imputing once
#>  before validation leaks information.<div style='margin-top: 10px;
#>  font-size: 12px; color: inherit; opacity: 0.8;'>*This assessment is
#>  advisory and cannot establish model adequacy. The analysis proceeds
#>  regardless of the verdict. Green = no issue detected by this limited
#>  check, Gray = adequacy not determined by this diagnostic, Yellow =
#>  proceed with caution, Red = results may be highly unstable or
#>  misspecified.*
#> 
#>  Model Summary                                   
#>  ─────────────────────────────────────────────── 
#>    Statistic                        Value        
#>  ─────────────────────────────────────────────── 
#>    Encoded Predictor Columns        9            
#>    Selected Predictor Columns       1            
#>    Column Selection Proportion      11.1%        
#>    Optimal Lambda                   5.58e-02     
#>    Penalty Selected By              lambda.min   
#>    Stratified CV Folds Used         10           
#>    Sample Size                      245          
#>    Number of Events                 85           
#>    Censoring Rate                   65.3%        
#>    Event Level Used                 Dead         
#>    Rows Excluded (Missing Data)     5            
#>    Original Candidate Predictors    7            
#>    Selected Original Predictors     1            
#>    Censored Level Used              Alive        
#>  ─────────────────────────────────────────────── 
#>    Note. Random seed: 123456
#> 
#> 
#>  Penalized Coefficients at Selected Lambda                                                                   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Variable        Penalized Coefficient    Penalized Hazard Ratio    Scale-Adjusted Magnitude |beta| x SD   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    ki67_percent                   0.0119                    1.0119                                  0.1799   
#>  ─────────────────────────────────────────────────────────────────────────────────────────────────────────── 
#>    Note. The selected columns depend on the cross-validation split, so a different seed or row order can
#>    select a different set.
#>    Note. Coefficient and Hazard Ratio are from the penalized LASSO Cox fit at the selected lambda.
#>    Conventional p-values and confidence intervals are intentionally not reported because same-data
#>    post-selection inference would not account for variable and penalty selection.
#>    Note. Scale-adjusted magnitude is |coefficient| multiplied by the complete-case SD of that encoded
#>    design column. It describes a one-SD change in the fitted linear predictor, not causal importance or
#>    selection stability; for a factor indicator it is not the category-versus-reference hazard ratio.
#>    Note. Predictors were standardized internally within glmnet fitting for the penalty calculation.
#>    glmnet back-transforms the displayed coefficients to the original design-column scale;
#>    indicator-column hazard ratios therefore compare that category with its reference category.
#>    Indicators are still penalized and selected separately rather than as a grouped factor.
#> 
#> 
#>  Apparent Development Performance                                               
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Metric              Value    Interpretation                                  
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Apparent C-index    0.62     Development data only; optimism not corrected   
#>  ────────────────────────────────────────────────────────────────────────────── 
#>    Note. This C-index is apparent (training) performance from the same
#>    patients used for preprocessing, penalty selection, and model fitting.
#>    Its uncertainty does not include the modeling process. Use bootstrap
#>    optimism correction or nested cross-validation that repeats all
#>    preprocessing and tuning, followed by external validation before
#>    clinical use.
#> 



# }
```
