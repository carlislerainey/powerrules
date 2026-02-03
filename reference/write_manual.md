# Generate manual calculation code for a power analysis

Prints copy-pasteable base R code that reproduces the power calculation
using the paper's rounded constants and critical values.

## Usage

``` r
write_manual(result)
```

## Arguments

- result:

  A `"power_result"` object from
  [`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md),
  [`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md),
  or
  [`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md).

## Value

The generated code as a character string (returned invisibly).

## Examples

``` r
from_sd(sd_y = 20.8) |> find_mde(n = 500) |> write_manual()
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
#> -- Manual Calculation -------------------------------------------------- 
#> 
#>   # Manual power calculation
#>   
#>   # --- Inputs ---
#>   sd_y <- 20.8   # SD of outcome in reference population
#>   n    <- 500   # respondents per condition
#>   
#>   # --- Predicted SE (Rule 3) ---
#>   se <- 2 * sd_y / sqrt(2 * n)
#>   
#>   # --- MDE (Rule 5) ---
#>   mde_80 <- 2.5 * se   # 80% power
#>   mde_95 <- 3.3 * se   # 95% power
#>   
#>   cat("Predicted SE:      ", round(se, 2), "\n")
#>   cat("MDE (80% power):   ", round(mde_80, 2), "\n")
#>   cat("MDE (95% power):   ", round(mde_95, 2), "\n")
```
