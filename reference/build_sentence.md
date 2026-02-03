# Build a manuscript sentence from a power result

Constructs a publication-ready sentence describing the power analysis.
Used internally by the print method and the Shiny app.

## Usage

``` r
build_sentence(x, effect_label, effect_article, n_cond)
```

## Arguments

- x:

  A `"power_result"` object.

- effect_label:

  "treatment effect" or "interaction".

- effect_article:

  "a treatment effect" or "an interaction".

- n_cond:

  Number of conditions (2 or 4).

## Value

A character string.
