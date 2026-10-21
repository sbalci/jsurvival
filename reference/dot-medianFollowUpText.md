# Formatted median follow-up value, with confidence interval when available

Formatted median follow-up value, with confidence interval when
available

## Usage

``` r
.medianFollowUpText(mfu, unit = "", conf_level = 0.95)
```

## Arguments

- mfu:

  Result of
  [`.medianFollowUp()`](https://www.serdarbalci.com/jsurvival/reference/dot-medianFollowUp.md).

- unit:

  Optional time unit appended to the value (e.g. `"months"`).

- conf_level:

  Confidence level, used only to label the interval.

## Value

Character scalar, e.g. `"25.4 months (95% CI 22.2 to 28.5)"`.
