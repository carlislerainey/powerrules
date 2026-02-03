# Browser-based power analysis guide

Opens a Shiny app that walks through a power analysis on a virtual
whiteboard. The app collects inputs, shows step-by-step formulas with
color-coded values, and produces a manuscript-ready sentence and a
copyable pipeline.

## Usage

``` r
shiny_rules()
```

## Value

Called for its side effect (launches the app). Returns the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
invisibly.
