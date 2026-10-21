# Median follow-up by the reverse Kaplan-Meier method

Estimates how long a cohort was actually observed, by swapping the roles
of event and censoring and fitting an ordinary Kaplan-Meier curve to the
result (Schemper & Smith 1996). The median of that reversed curve
estimates the median of the potential-follow-up distribution: the time
each subject would have been observed had the event not intervened.

## Usage

``` r
.medianFollowUp(time, censored, conf_level = 0.95)
```

## Arguments

- time:

  Numeric vector of observed times (event or censoring).

- censored:

  Logical or 0/1 vector, `TRUE`/`1` where the subject was still under
  observation when follow-up ended. Recycled length-1 values are not
  accepted; it must be parallel to `time`.

- conf_level:

  Confidence level for the interval around the median. Default 0.95.

## Value

A list with `value` (the estimate), `ci_lower` / `ci_upper` (`NA` unless
the reverse fit succeeded and the interval is defined), `reverse`
(`TRUE` when the reverse-KM median was estimable), `method`
(`"reverse_km"` or `"observed_median"`), `reason` (empty when
`reverse = TRUE`, otherwise why the fallback was used), `n_total` and
`n_censored`.

## Details

`censored` must mark subjects whose observation ended WITHOUT a terminal
outcome – alive at last contact, lost to follow-up, administratively
censored. In a competing-risks setting a competing death is a terminal
outcome and must NOT be marked censored here: it ends potential
follow-up just as the event of interest does, and counting it as a
reverse-KM event understates the reported follow-up.

The median is undefined whenever the reversed curve never falls to 50%.
That depends on WHEN subjects were still under observation, not simply
on how many were: a handful censored late can make the median estimable,
while a cohort whose censored subjects all left early cannot – so do not
read a fallback as meaning "too little censoring". It is a real and
common state, not an error, so the function falls back to the plain
median of observed times and reports `reverse = FALSE` plus a `reason`.
Callers MUST surface that distinction – printing the fallback under a
"reverse Kaplan-Meier" label is a quiet lie. Use
[`.medianFollowUpLabel()`](https://www.serdarbalci.com/jsurvival/reference/dot-medianFollowUpLabel.md)
rather than hardcoding a label.

## References

Schemper, M., & Smith, T. L. (1996). A note on quantifying follow-up in
studies of failure time. Controlled Clinical Trials, 17(4), 343-346.
[doi:10.1016/0197-2456(96)00075-X](https://doi.org/10.1016/0197-2456%2896%2900075-X)
