# Computing power with a for-loop simulation

[`write_loop()`](https://carlislerainey.github.io/powerrules/reference/write_loop.md)
generates copy-pasteable R code for a for-loop simulation that confirms
the analytical power calculation. The simulation uses potential outcomes
with a constant additive treatment effect and HC2 robust standard errors
(via the sandwich package).

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

## Generating the simulation code

[`write_loop()`](https://carlislerainey.github.io/powerrules/reference/write_loop.md)
takes the result and prints a self-contained simulation script:

``` r

write_loop(result)
```

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

## Running the generated code

The simulation runs 500 experiments with one-sided tests (size 0.05 for
90% CIs), matching Rule 2 from Rainey (2026) exactly. The simulated
power should be close to the analytical 74% from powerrules.

``` r

library(sandwich)  # for HC2 robust SEs

# --- Parameters ---
n    <- 500    # respondents per condition
tau  <- 3      # treatment effect
sd_y <- 20.8   # SD(Y) in control group

# --- Simulate 500 experiments ---
n_sims <- 500
set.seed(1234)
reject <- logical(n_sims)

for (i in seq_len(n_sims)) {
  # Potential outcomes: constant, additive treatment effect
  y0 <- rnorm(2 * n, mean = 0, sd = sd_y)
  y1 <- y0 + tau

  # Complete random assignment: n to each condition
  z <- sample(rep(0:1, each = n))

  # Reveal observed outcome
  y <- ifelse(z == 1, y1, y0)

  # Estimate via OLS with HC2 robust SEs
  fit <- lm(y ~ z)
  est <- coef(fit)["z"]
  se  <- sqrt(vcovHC(fit, type = "HC2")["z", "z"])

  # One-sided p-value and size-0.05 test
  p_val <- pnorm(est / se, lower.tail = FALSE)
  reject[i] <- p_val < 0.05
}

cat("Simulated power:", round(mean(reject) * 100), "%\n")
```

    #> Simulated power: 77 %

``` r

cat("Expected:       ~74%\n")
```

    #> Expected:       ~74%

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *Journal
of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Statistical Power.” *Political Analysis*.
<https://osf.io/preprints/osf/5am9q>.
