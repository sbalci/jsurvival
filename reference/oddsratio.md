# Odds Ratio Table and Plot

Logistic regression odds-ratio table, forest plot, prediction nomogram,
and optional binary diagnostic metrics.

## Usage

``` r
oddsratio(
  data,
  explanatory = NULL,
  outcome = NULL,
  outcomeLevel,
  outcomeNegativeLevel,
  diagnosticPredictor = NULL,
  predictorLevel,
  predictorNegativeLevel,
  usePenalized = FALSE,
  showNomogram = FALSE,
  showExplanations = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- explanatory:

  The explanatory variables to be used in the analysis.

- outcome:

  The outcome variable to be used in the analysis.

- outcomeLevel:

  Specify which outcome level is modeled as the event in logistic
  regression and used as positive for diagnostic metrics. Both
  outcomeLevel and outcomeNegativeLevel are required; until both are
  specified the analysis asks for them and computes nothing. They are
  never taken from the order of the levels, and any other levels are
  excluded.

- outcomeNegativeLevel:

  Specify which outcome level is modeled as the non-event (reference, 0)
  in logistic regression and used as negative for diagnostic metrics.
  Both outcomeLevel and outcomeNegativeLevel are required; until both
  are specified the analysis asks for them and computes nothing. Any
  rows with other outcome levels are excluded with a warning.

- diagnosticPredictor:

  Specify the predictor to drive likelihood ratios. Defaults to the
  first explanatory variable when predictorLevel and
  predictorNegativeLevel are supplied in R; in the jamovi GUI, populate
  this box to enable the Predictor Positive Level selector (and
  Predictor Negative Level selector).

- predictorLevel:

  Which level of the diagnostic predictor is the positive test result.
  Both predictorLevel and predictorNegativeLevel are required for
  diagnostic metrics (sensitivity, specificity, likelihood ratios) and
  are never taken from the level names or their order. Because a Level
  option cannot declare a default, this is a required argument of the R
  function: predictorLevel = NULL leaves the diagnostic metrics waiting
  for the choice while the logistic model, odds ratios, forest plot and
  nomogram still run.

- predictorNegativeLevel:

  Which level of the diagnostic predictor is the negative test result.
  Both predictorLevel and predictorNegativeLevel are required for
  diagnostic metrics (sensitivity, specificity, likelihood ratios) and
  are never taken from the level names or their order; any other
  predictor levels are excluded from the 2x2 table with a warning.
  Because a Level option cannot declare a default, this is a required
  argument of the R function: predictorNegativeLevel = NULL leaves the
  diagnostic metrics waiting for the choice while the logistic model,
  odds ratios, forest plot and nomogram still run.

- usePenalized:

  Use Firth penalized likelihood logistic regression. This is
  recommended when there is separation (zero cells), small sample sizes,
  or low events-per-variable.

- showNomogram:

  Display a prediction nomogram from the maximum-likelihood logistic
  model and, when a binary diagnostic predictor is available, separate
  unadjusted sensitivity, specificity, and likelihood-ratio estimates.
  The prediction nomogram is not generated when Firth penalized
  regression is selected.

- showExplanations:

  Display educational explanations for odds ratios, their distinction
  from risk ratios, binary diagnostic-test performance, likelihood
  ratios, and prediction nomograms.

## Value

A results object containing:

|                                       |     |     |     |     |          |
|---------------------------------------|-----|-----|-----|-----|----------|
| `results$todo`                        |     |     |     |     | a html   |
| `results$errors`                      |     |     |     |     | a html   |
| `results$strongWarnings`              |     |     |     |     | a html   |
| `results$warnings`                    |     |     |     |     | a html   |
| `results$infoMessages`                |     |     |     |     | a html   |
| `results$fittedTable`                 |     |     |     |     | a table  |
| `results$text`                        |     |     |     |     | a html   |
| `results$text2`                       |     |     |     |     | a html   |
| `results$plot`                        |     |     |     |     | an image |
| `results$oddsRatioExplanation`        |     |     |     |     | a html   |
| `results$riskMeasuresExplanation`     |     |     |     |     | a html   |
| `results$diagnosticTestExplanation`   |     |     |     |     | a html   |
| `results$plot_nomogram`               |     |     |     |     | an image |
| `results$diagnosticContingency`       |     |     |     |     | a table  |
| `results$diagnosticMetrics`           |     |     |     |     | a html   |
| `results$nomogram`                    |     |     |     |     | a html   |
| `results$nomogramAnalysisExplanation` |     |     |     |     | a html   |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$fittedTable$asDF`

`as.data.frame(results$fittedTable)`
