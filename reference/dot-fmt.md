# Interpolate a translated string without risking an unbounded substitution loop

[`jmvcore::format()`](https://rdrr.io/pkg/jmvcore/man/format.html)
re-scans the ENTIRE string from position 1 after each substitution. If a
substituted value contains its own placeholder – e.g.
`jmvcore::format("LR ({value})", value = "x {value} y")` – the
substituter finds that placeholder again, substitutes again, and never
terminates. The loop runs in code that does not poll R's interrupt
handler, so it survives
[`setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html) and has to
be SIGKILLed; inside jamovi it freezes the analysis engine rather than
raising an error.

## Usage

``` r
.fmt(.format_string, ...)
```

## Arguments

- .format_string:

  Format string, normally wrapped in `.()`.

- ...:

  Named placeholder values.

## Value

The interpolated string.

## Details

Two realistic ways in: a translator copies a `{placeholder}` into the
msgstr of the very string that placeholder belongs to, or a dataset
carries a column/level named literally `{n}` that is then interpolated
by name.

This wrapper is a pass-through. When no supplied value contains a brace
– the overwhelming majority of calls – it delegates untouched and the
output is byte-identical to calling
[`jmvcore::format()`](https://rdrr.io/pkg/jmvcore/man/format.html)
directly. Only when a value actually contains an opening brace are that
value's braces neutralised, so a pathological input degrades to slightly
different text instead of hanging.

Verified trigger conditions (R, jmvcore 2.7.x): a value containing its
OWN placeholder name hangs; a value containing a DIFFERENT supplied name
substitutes and terminates; an UNKNOWN `{name}` renders as an ellipsis;
a bare [`{ }`](https://rdrr.io/r/base/Paren.html) is left literal.
