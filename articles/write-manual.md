# Computing power with manual calculations

[`write_manual()`](https://carlislerainey.github.io/powerrules/reference/write_manual.md)
generates copy-pasteable base R code that reproduces a powerrules
calculation using the paper’s constants (2.5 for 80% power, 3.3 for 95%
power, 1.64 for the critical value with 90% CIs). This lets researchers
verify the computation without any dependencies beyond base R.

## Running the pipeline

Suppose we plan to replicate Ahler and Sood (2018), who find that
correcting respondents’ misperceptions of their out-party reduces
affective polarization. The standard deviation of feeling thermometer
responses in the 2020 ANES is 20.8. We plan to recruit 500 respondents
per condition and assume a treatment effect of 3 points (the lower bound
of Ahler and Sood’s 95% confidence interval).

``` r
library(powerrules)

result <- from_sd(sd_y = 20.8) |>
  find_power(n = 500, tau = 3)
```

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

## Generating the manual calculation code

[`write_manual()`](https://carlislerainey.github.io/powerrules/reference/write_manual.md)
takes the result and prints a self-contained base R script:

``` r
write_manual(result)
```

    #> -- Manual Calculation -------------------------------------------------- 
    #> 
    #>   # Manual power calculation
    #>   
    #>   # --- Inputs ---
    #>   sd_y <- 20.8   # SD of outcome in reference population
    #>   n    <- 500   # respondents per condition
    #>   tau  <- 3   # assumed treatment effect
    #>   
    #>   # --- Predicted SE (Rule 3) ---
    #>   se <- 2 * sd_y / sqrt(2 * n)
    #>   
    #>   # --- Power (Rule 2) ---
    #>   power <- 1 - pnorm(1.64 - tau / se)
    #>   
    #>   cat("Predicted SE:        ", round(se, 2), "\n")
    #>   cat("tau / SE:            ", round(tau / se, 2), "\n")
    #>   cat("Power:               ", round(power * 100), "%\n")

## Running the generated code

The generated code computes the predicted SE and power using base R
only. The result matches the analytical 74% from powerrules.

``` r
# Manual power calculation

# --- Inputs ---
sd_y <- 20.8   # SD of outcome in reference population
n    <- 500    # respondents per condition
tau  <- 3      # assumed treatment effect

# --- Predicted SE (Rule 3) ---
se <- 2 * sd_y / sqrt(2 * n)

# --- Power (Rule 2) ---
power <- 1 - pnorm(1.64 - tau / se)

cat("Predicted SE:        ", round(se, 2), "\n")
```

    #> Predicted SE:         1.32

``` r
cat("tau / SE:            ", round(tau / se, 2), "\n")
```

    #> tau / SE:             2.28

``` r
cat("Power:               ", round(power * 100), "%\n")
```

    #> Power:                74 %

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.
