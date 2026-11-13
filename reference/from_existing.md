# Create a power source from an existing study

Uses Pilot Rule 1: Rule 1 of the companion paper to Rainey (2026),
"Automating Power Analysis with Pilot Data." The SE and sample size from
an existing study are used to predict the SE of the planned study.

## Usage

``` r
from_existing(se_existing, n_existing, interaction = FALSE)
```

## Arguments

- se_existing:

  Standard error from the existing study.

- n_existing:

  Respondents per condition in the existing study.

- interaction:

  Logical; TRUE for a 2x2 factorial interaction. The SE computation is
  unchanged (the existing SE already encodes the design), but totals,
  design labels, and manuscript sentences will reflect the factorial
  structure.

## Value

An object of class `"power_source"`.

## Examples

``` r
from_existing(se_existing = 1.8, n_existing = 268) |>
  find_mde(n_planned = 500)
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     existing study
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SE (existing) = 1.8 
#>     n (existing)  = 268 per condition
#>     n (planned)   = 500 per condition (1,000 total)
#> 
#>   Predicted SE = sqrt(268 / 500) * 1.8 = 1.32             [Pilot Rule 1]
#>   MDE (80% power) = 2.49 * 1.32 = 3.28                          [Rule 5] 
#>   MDE (95% power) = 3.29 * 1.32 = 4.34                          [Rule 5] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design with 500 respondents per
#>   condition (1,000 total) replicating an existing study with a standard
#>   error of 1.8 (268 per condition), the predicted standard error is
#>   1.32. Using a one-sided test at the 0.05 level, the experiment has 80%
#>   power to detect a treatment effect of 3.28 units and 95% power to
#>   detect a treatment effect of 4.34 units. 
#> 
#>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
#>   95% power. This software uses exact values (2.49 and 3.29), so results
#>   differ slightly from hand calculations using the rounded factors. 
```
