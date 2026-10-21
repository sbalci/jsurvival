# Backtick-quote variable names for use in a formula string

The inverse of
[`.stripBackticks()`](https://www.serdarbalci.com/jsurvival/reference/dot-stripBackticks.md):
takes raw jamovi variable names and returns them quoted wherever R would
otherwise fail to parse them as a symbol. Use it when BUILDING a formula
string, never as a `data[[...]]` key – a backticked name is not a column
name, and the lookup returns `NULL`.

## Usage

``` r
.escapeVariableNames(var_names)
```

## Arguments

- var_names:

  Character vector of raw variable names.

## Value

Character vector, quoted where quoting is needed. Length and order are
preserved, so it is safe in
[`paste()`](https://rdrr.io/r/base/paste.html)/[`paste0()`](https://rdrr.io/r/base/paste.html)
alongside other parallel vectors.

## Details

This delegates to
[`jmvcore::composeTerm()`](https://rdrr.io/pkg/jmvcore/man/decomposeTerm.html)
rather than testing `grepl("[^a-zA-Z0-9._]", x)` and wrapping in
backticks by hand, which is what it did until 2026-09-06. That
hand-rolled rule quoted only names containing a character outside
`[A-Za-z0-9._]`, and so silently produced formulas that do not parse –
or, worse, parse into something else:

- `1stGrade` – leading digit, no "special" character, left unquoted; the
  formula fails to parse.

- `if`, `for`, `function` – reserved words, left unquoted; parse error.

- `` a`b `` – wrapped to produce `` `a`b` ``, three backticks; parse
  error.

- `TRUE` – left unquoted, so the term parses as the literal `TRUE`
  rather than as the column. That one is the dangerous case: it fails
  silently.

[`jmvcore::composeTerm()`](https://rdrr.io/pkg/jmvcore/man/decomposeTerm.html)
is jamovi's own escaper, handles all four, and agrees with the old rule
on every name the old rule got right – so this is a strict improvement,
not a behaviour change, for existing analyses.

Matching backticked names against fitted-model coefficients (as
`multisurvival-interactions.R` does) stays correct: `coxph()` derives
its coefficient names by deparsing the same terms, and `composeTerm()`'s
output has been verified to match those names for plain, spaced,
punctuated and digit-leading variable names alike.
