# Create a power source from a reference population SD

Uses Rules 3 and 4 from Rainey (2026). The SD of the outcome in a
reference population is used to predict the SE of the planned study.

## Usage

``` r
from_sd(sd_y, r_squared = 0, interaction = FALSE)
```

## Arguments

- sd_y:

  Standard deviation of the outcome in a reference population.

- r_squared:

  R-squared of control variables (default 0, no adjustment).

- interaction:

  Logical; TRUE for a 2x2 factorial interaction.

## Value

An object of class `"power_source"`.

## Examples

``` r
from_sd(sd_y = 20.8) |> find_mde(n = 500)
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     reference population SD
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SD(Y) = 20.8 
#>     n     = 500 per condition (1,000 total)
#> 
#>   Predicted SE = 2 * 20.8 / sqrt(2 * 500) = 1.32                [Rule 3]
#>   MDE (80% power) = 2.49 * 1.32 = 3.27                          [Rule 5] 
#>   MDE (95% power) = 3.29 * 1.32 = 4.33                          [Rule 5] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design with 500 respondents per
#>   condition (1,000 total), assuming a standard deviation of 20.8, the
#>   predicted standard error is 1.32. Using a one-sided test at the 0.05
#>   level, the experiment has 80% power to detect a treatment effect of
#>   3.27 units and 95% power to detect a treatment effect of 4.33 units. 
#> 
#>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
#>   95% power. This software uses exact values (2.49 and 3.29), so results
#>   differ slightly from hand calculations using the rounded factors. 
```
