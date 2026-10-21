# Run third-party code without leaking package chatter into the results

jamovi's engine captures
[`message()`](https://rdrr.io/r/base/message.html) and
[`warning()`](https://rdrr.io/r/base/warning.html) conditions raised
while an analysis runs and renders them in the "Analysis Notes" panel,
where users see them. Third-party modelling packages emit a lot of
chatter that is meaningless to a pathologist reading their results –
glmnet's `cox.ties` migration notice, for example, appeared twelve times
in a single Lasso-Cox run.

## Usage

``` r
.quietly(
  expr,
  deprecation_pattern = paste("deprecat", "defunct", "superseded", "will change from",
    "is no longer", "renamed", "future version", "startup", sep = "|")
)
```

## Arguments

- expr:

  Expression to evaluate.

- deprecation_pattern:

  Regex matched against warning messages; matches are muffled. Defaults
  to the usual deprecation/migration vocabulary.

## Value

The value of `expr`.

## Details

Wrap a third-party call in `.quietly()` to keep that noise out of the
results pane. It suppresses ALL messages (package chatter is never the
user's problem) but muffles only *deprecation-flavoured* warnings,
matched by `deprecation_pattern`. Substantive warnings –
non-convergence, NAs introduced, rank deficiency – still propagate,
because those change how the output should be read and must not be
hidden.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- .quietly(glmnet::cv.glmnet(x, y, family = "cox"))
} # }
```
