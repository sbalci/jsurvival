# Lasso-Cox Regression for Variable Selection in Survival Analysis

Performs Lasso-penalized Cox proportional hazards regression for
variable selection in survival analysis. This function uses penalized
likelihood to shrink coefficients and produce a sparse candidate model.
Regularization can reduce, but does not eliminate, overfitting;
development results require internal and external validation before
clinical use.

## Value

An `R6` class generator object for the `lassocoxClass` backend; used
internally by the jamovi analysis wrapper and not called directly.

## Details

The Lasso-Cox regression combines the Cox proportional hazards model
with L1 regularization (Lasso penalty) to estimate a sparse coefficient
vector. The method minimizes the partial likelihood penalized by the L1
norm of the coefficient vector, shrinking coefficients toward zero and
setting some exactly to zero. A zero coefficient is a model-selection
result in these data and does not establish that a predictor is
clinically irrelevant.

Key features:

- Automatic variable selection through L1 regularization

- Cross-validation for optimal tuning parameter selection

- Risk score calculation and stratification

- Apparent development-sample discrimination

- Exploratory survival-curve visualization by a development-sample
  median split

- Optional coefficient paths, encoding tables, fold provenance, and
  upstream R code

Nominal and ordered factors use explicit treatment indicators, with the
first observed level in the factor order as reference. Global or custom
contrasts are not used. Ordered factors are not assumed to follow
polynomial trends. Original predictors and encoded columns are counted
separately. Nonpositive follow-up times, infinite predictor values, and
predictors containing only missing values are rejected. Time and outcome
variables cannot also be predictors.

The optional R code refits the same development data using upstream
glmnet; it is not external validation or an absolute-risk calculator.
Pairwise correlation diagnostics are skipped above 500 encoded columns,
without changing the model.

The function uses the glmnet package for efficient computation and
supports both lambda.min (minimum cross-validation error) and lambda.1se
(1 standard error rule) for tuning parameter selection.

## Super classes

[`jmvcore::Analysis`](https://rdrr.io/pkg/jmvcore/man/Analysis.html) -\>
`lassocoxBase` -\> `lassocoxClass`

## Methods

### Public methods

- [`lassocoxClass$clone()`](#method-lassocoxClass-clone)

Inherited methods

- [`jmvcore::Analysis$.createImage()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImage)
- [`jmvcore::Analysis$.createImages()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createImages)
- [`jmvcore::Analysis$.createPlotObject()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.createPlotObject)
- [`jmvcore::Analysis$.getSessionTemp()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.getSessionTemp)
- [`jmvcore::Analysis$.load()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.load)
- [`jmvcore::Analysis$.render()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.render)
- [`jmvcore::Analysis$.save()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.save)
- [`jmvcore::Analysis$.savePart()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.savePart)
- [`jmvcore::Analysis$.setCheckpoint()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setCheckpoint)
- [`jmvcore::Analysis$.setParent()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setParent)
- [`jmvcore::Analysis$.setReadDatasetHeaderSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetHeaderSource)
- [`jmvcore::Analysis$.setReadDatasetSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setReadDatasetSource)
- [`jmvcore::Analysis$.setResourcesPathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setResourcesPathSource)
- [`jmvcore::Analysis$.setStatePathSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-.setStatePathSource)
- [`jmvcore::Analysis$addAddon()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-addAddon)
- [`jmvcore::Analysis$asProtoBuf()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asProtoBuf)
- [`jmvcore::Analysis$asSource()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-asSource)
- [`jmvcore::Analysis$check()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-check)
- [`jmvcore::Analysis$init()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-init)
- [`jmvcore::Analysis$optionsChangedHandler()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-optionsChangedHandler)
- [`jmvcore::Analysis$postInit()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-postInit)
- [`jmvcore::Analysis$print()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-print)
- [`jmvcore::Analysis$readDataset()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-readDataset)
- [`jmvcore::Analysis$run()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-run)
- [`jmvcore::Analysis$serialize()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-serialize)
- [`jmvcore::Analysis$setError()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setError)
- [`jmvcore::Analysis$setStatus()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-setStatus)
- [`jmvcore::Analysis$translate()`](https://rdrr.io/pkg/jmvcore/man/Analysis.html#method-translate)
- `lassocoxBase$initialize()`

------------------------------------------------------------------------

### `lassocoxClass$clone()`

The objects of this class are cloneable with this method.

#### Usage

    lassocoxClass$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Both outcomeLevel and censorLevel are `type: Level` options. jamovi forbids a
# default on a Level, so both are REQUIRED arguments of this function even though
# the GUI fills them in for you - pass them explicitly when calling from R.
# Basic Lasso-Cox regression
result <- lassocox(
  data = survival_data,
  elapsedtime = "time",
  outcome = "status", 
  outcomeLevel = "1",
  censorLevel = "0",
  explanatory = c("age", "gender", "stage", "grade"),
  lambda = "lambda.1se",
  nfolds = 10
)

# High-dimensional scenario
result <- lassocox(
  data = genomic_data,
  elapsedtime = "survival_time",
  outcome = "event",
  outcomeLevel = "death",
  censorLevel = "alive",
  explanatory = gene_variables,
  lambda = "lambda.min",
  nfolds = 5,
  standardize = TRUE
)
} # }
```
