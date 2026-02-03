# Generate for-loop simulation code for a power analysis

Prints copy-pasteable R code for a for-loop simulation that confirms the
analytical power calculation. Uses potential outcomes with a constant
additive treatment effect and HC2 robust standard errors (via the
sandwich package).

## Usage

``` r
write_loop(result, tau = NULL)
```

## Arguments

- result:

  A `"power_result"` object from
  [`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md),
  [`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md),
  or
  [`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md).

- tau:

  Effect size for the simulation. Defaults to `result$tau` for
  `find_power`/`find_n` results, or `result$mde_80` for `find_mde`
  results (with a note).

## Value

The generated code as a character string (returned invisibly).

## Examples

``` r
from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3) |> write_loop()
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
#> -- For-Loop Simulation ------------------------------------------------- 
#> 
#>   # For-loop power simulation
#>   # Confirms the analytical power calculation via simulation.
#>   
#>   library(sandwich)  # for HC2 robust SEs
#>   
#>   # --- Parameters ---
#>   n    <- 500    # respondents per condition
#>   tau  <- 3      # treatment effect
#>   sd_y <- 20.8   # SD(Y) in control group
#>   
#>   # --- Simulate 500 experiments ---
#>   n_sims <- 500
#>   set.seed(1234)
#>   reject <- logical(n_sims)
#>   
#>   for (i in seq_len(n_sims)) {
#>     # Potential outcomes: constant, additive treatment effect
#>     y0 <- rnorm(2 * n, mean = 0, sd = sd_y)
#>     y1 <- y0 + tau
#>   
#>     # Complete random assignment: n to each condition
#>     z <- sample(rep(0:1, each = n))
#>   
#>     # Reveal observed outcome
#>     y <- ifelse(z == 1, y1, y0)
#>   
#>     # Estimate via OLS with HC2 robust SEs
#>     fit <- lm(y ~ z)
#>     est <- coef(fit)["z"]
#>     se  <- sqrt(vcovHC(fit, type = "HC2")["z", "z"])
#>   
#>     # One-sided p-value and size-0.05 test
#>     p_val <- pnorm(est / se, lower.tail = FALSE)
#>     reject[i] <- p_val < 0.05
#>   }
#>   
#>   cat("Simulated power:", round(mean(reject) * 100), "%\n")
#>   # Expected: ~74%
```
