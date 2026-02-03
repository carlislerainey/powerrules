# Create a power source from pilot data

Uses Rule 9 from Rainey (2026). The SE and sample size from a pilot
study are used to predict the SE of the planned study, with a
conservative adjustment for pilot noise.

## Usage

``` r
from_pilot(se_pilot, n_pilot, interaction = FALSE)
```

## Arguments

- se_pilot:

  Standard error from the pilot study.

- n_pilot:

  Respondents per condition in the pilot study.

- interaction:

  Logical; TRUE for a 2x2 factorial interaction (changes the
  conservative adjustment factor).

## Value

An object of class `"power_source"`.

## Examples

``` r
from_pilot(se_pilot = 2.13, n_pilot = 85) |>
  find_mde(n_planned = 500)
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     pilot data (conservative)
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SE (pilot) = 2.13 
#>     n (pilot)  = 85 per condition
#>     n (planned)   = 500 per condition (1,000 total)
#> 
#>   Predicted SE = sqrt(85 / 500) * (sqrt(1/85) + 1) * 2.13 = 0.97  [Rule 9]
#>   MDE (80% power) = 2.49 * 0.97 = 2.42                          [Rule 5] 
#>   MDE (95% power) = 3.29 * 0.97 = 3.20                          [Rule 5] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design with 500 respondents per
#>   condition (1,000 total), using pilot data with a standard error of
#>   2.13 (85 per condition) and a conservative adjustment for pilot noise,
#>   the predicted standard error is 0.97. Using a one-sided test at the
#>   0.05 level, the experiment has 80% power to detect a treatment effect
#>   of 2.42 units and 95% power to detect a treatment effect of 3.20
#>   units. 
#> 
#>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
#>   95% power. This software uses exact values (2.49 and 3.29), so results
#>   differ slightly from hand calculations using the rounded factors. 
```
