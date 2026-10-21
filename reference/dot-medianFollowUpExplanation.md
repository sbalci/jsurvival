# HTML explanation of how median follow-up was calculated and why

Rendered next to the estimate so a clinician reading the output can tell
what the number means, which estimator produced it, and – when the
reverse-KM median was not estimable – exactly why the fallback appears
instead.

## Usage

``` r
.medianFollowUpExplanation(mfu, unit = "", conf_level = 0.95)
```

## Arguments

- mfu:

  Result of
  [`.medianFollowUp()`](https://www.serdarbalci.com/jsurvival/reference/dot-medianFollowUp.md).

- unit:

  Optional time unit (e.g. `"months"`).

- conf_level:

  Confidence level used for the interval.

## Value

An HTML string.

## Details

Colours are expressed as translucent tints over the host background
rather than opaque hex fills, so the block stays readable in jamovi's
dark theme.
