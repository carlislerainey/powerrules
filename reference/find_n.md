# Find the required sample size

Given a power source, assumed treatment effect, and desired power level,
computes the required sample size per condition. Uses Rule 6 from Rainey
(2026) with
[`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md),
and Pilot Rules 2 and 4 (Rules 2 and 4 of the companion paper,
"Automating Power Analysis with Pilot Data") with
[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
and
[`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md).

## Usage

``` r
find_n(
  source,
  tau,
  power = 0.8,
  ci = 0.9,
  quiet = getOption("powerrules.quiet", FALSE)
)
```

## Arguments

- source:

  A `"power_source"` object.

- tau:

  The assumed treatment effect (positive).

- power:

  Desired power level (default 0.80). Must be greater than the size of
  the test (0.05 for a 90 percent confidence interval, 0.025 for a 95
  percent confidence interval).

- ci:

  Confidence interval level: 0.90 (default) or 0.95.

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
from_sd(sd_y = 20.8) |> find_n(tau = 3)
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     reference population SD
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SD(Y) = 20.8 
#>     tau   = 3
#>     power = 80% 
#> 
#>   MDE factor          = qnorm(0.95) + qnorm(0.80) = 2.49       [Table 1] 
#>   n (planned)         = 2 * (2.49 * 20.8 / 3)^2
#>                       = 595 per condition (1,190 total)         [Rule 6] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design, assuming a standard deviation
#>   of 20.8, the experiment requires 595 respondents per condition (1,190
#>   total) for 80% power to detect a treatment effect of 3 units, using a
#>   one-sided test at the 0.05 level. 
#> 
#>   Note: The paper rounds the MDE factor to 2.5 for 80% power. This
#>   software uses the exact value (2.49), so results differ slightly from
#>   hand calculations using the rounded factor. 
```
