# Label for a median follow-up estimate

Names the quantity that was actually computed. When the reverse-KM
median was not estimable the returned label says "median observed time",
because calling the fallback a reverse-KM follow-up misrepresents it.

## Usage

``` r
.medianFollowUpLabel(mfu)
```

## Arguments

- mfu:

  Result of
  [`.medianFollowUp()`](https://www.serdarbalci.com/jsurvival/reference/dot-medianFollowUp.md).

## Value

Character scalar.
