# Find the required sample size

Given a power source, assumed treatment effect, and desired power level,
computes the required sample size per condition (Rules 6/8/10 from
Rainey, 2026).

## Usage

``` r
find_n(source, tau, power = 0.8, ci = 0.9)
```

## Arguments

- source:

  A `"power_source"` object.

- tau:

  The assumed treatment effect (positive).

- power:

  Desired power level (default 0.80).

- ci:

  Confidence interval level: 0.90 (default) or 0.95.

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
#>   MDE factor          = qnorm(0.95) + qnorm(0.80) = 2.49       [Table 2] 
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
