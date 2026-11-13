# Find the power for a given effect

Given a power source, planned sample size, and assumed treatment effect,
computes the power (Rule 2 from Rainey, 2026).

## Usage

``` r
find_power(
  source,
  n_planned = NULL,
  tau,
  ci = 0.9,
  n = NULL,
  quiet = getOption("powerrules.quiet", FALSE)
)
```

## Arguments

- source:

  A `"power_source"` object from
  [`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md),
  [`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md),
  or
  [`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md).

- n_planned:

  Respondents per condition in the planned study (a whole number). Use
  `n_planned` when the source is
  [`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
  or
  [`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md);
  use `n` when the source is
  [`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md).

- tau:

  The assumed treatment effect (positive).

- ci:

  Confidence interval level: 0.90 (default) or 0.95.

- n:

  Alias for `n_planned`. Specify one or the other, not both.

- quiet:

  If `TRUE`, compute the result without printing it. Defaults to
  `getOption("powerrules.quiet", FALSE)`, so
  `options(powerrules.quiet = TRUE)` silences every goal function.
  [`print()`](https://rdrr.io/r/base/print.html) the returned object to
  see the output later.

## Value

A `"power_result"` object (returned invisibly).

## Examples

``` r
from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     reference population SD
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SD(Y) = 20.8 
#>     n     = 500 per condition (1,000 total)
#>     tau   = 3
#> 
#>   Predicted SE = 2 * 20.8 / sqrt(2 * 500) = 1.32                [Rule 3]
#>   tau / SE     = 3 / 1.32 = 2.28
#>   Power        = 1 - pnorm(1.64 - 2.28) = 74%                   [Rule 2] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design with 500 respondents per
#>   condition (1,000 total), assuming a standard deviation of 20.8, the
#>   predicted standard error is 1.32. Using a one-sided test at the 0.05
#>   level, the experiment has 74% power to detect a treatment effect of 3
#>   units. 
```
