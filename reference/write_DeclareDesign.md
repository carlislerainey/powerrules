# Generate DeclareDesign code for a power analysis

Prints copy-pasteable DeclareDesign code that reproduces the power
calculation through simulation. The generated code follows Declaration
18.1 in Blair, Coppock, and Humphreys (2023).

## Usage

``` r
write_DeclareDesign(result, tau = NULL)
```

## Arguments

- result:

  A `"power_result"` object from
  [`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md),
  [`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md),
  or
  [`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md).

- tau:

  Effect size for the DeclareDesign simulation. Defaults to `result$tau`
  for `find_power`/`find_n` results, or `result$mde_80` for `find_mde`
  results (with a note).

## Value

The generated code as a character string (returned invisibly).

## Examples

``` r
from_sd(sd_y = 20.8) |> find_power(n = 268, tau = 2.5) |> write_DeclareDesign()
#> -- Power Analysis ------------------------------------------------------ 
#>   Design:     balanced, between-subjects
#>   Source:     reference population SD
#>   CI level:   90% (size-0.05 test of directional hypothesis)
#> 
#>   Inputs:
#>     SD(Y) = 20.8 
#>     n     = 268 per condition (536 total)
#>     tau   = 2.5
#> 
#>   Predicted SE = 2 * 20.8 / sqrt(2 * 268) = 1.80                [Rule 3]
#>   tau / SE     = 2.5 / 1.80 = 1.39
#>   Power        = 1 - pnorm(1.64 - 1.39) = 40%                   [Rule 2] 
#> 
#> -- Manuscript sentence (edit as needed) -------------------------------- 
#>   For a balanced, between-subjects design with 268 respondents per
#>   condition (536 total), assuming a standard deviation of 20.8, the
#>   predicted standard error is 1.80. Using a one-sided test at the 0.05
#>   level, the experiment has 40% power to detect a treatment effect of
#>   2.5 units. 
#> -- DeclareDesign Code (Declaration 18.1) ------------------------------- 
#> 
#>   library(DeclareDesign)
#>   
#>   # Declare a two-arm randomized experiment and diagnose its power
#>   # via simulation (see Declaration 18.1 in Blair, Coppock, and
#>   # Humphreys, 2023, Ch. 18).
#>   
#>   my_design <-
#>     # Model: 536 total respondents (268 per condition).
#>     # Y = tau * Z + noise, where SD(Y) = 20.8 in the
#>     # control group (Rule 3).
#>     declare_model(
#>       N = 536,
#>       U = rnorm(N),
#>       potential_outcomes(Y ~ 2.5 * Z + 20.8 * U)
#>     ) +
#>     # Inquiry: the average treatment effect.
#>     declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
#>     # Data strategy: complete random assignment, half to treatment.
#>     declare_assignment(Z = complete_ra(N, prob = 0.5)) +
#>     declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
#>     # Answer strategy: difference-in-means.
#>     declare_estimator(Y ~ Z, inquiry = "ATE")
#>   
#>   # Simulate 500 experiments.
#>   #   IMPORTANT NOTE: DeclareDesign does not easily
#>   #   support one-sided tests, so power below uses two-sided
#>   #   tests. This closely matches the directional power from
#>   #   powerrules (Rule 2) above ~20% power. Below that, the
#>   #   two-sided power will be somewhat higher because it counts
#>   #   significant results in the wrong direction as well.
#>   diagnosis <- diagnose_design(my_design, sims = 500,
#>     diagnosands = declare_diagnosands(
#>       power = mean(p.value <= 0.10)
#>     ))
#>   diagnosis
#> 
#> -- Note on power definition -------------------------------------------- 
#> 
#>   DeclareDesign does not easily support one-sided tests, so power above
#>   uses two-sided tests. This closely matches the directional power from
#>   powerrules (Rule 2) above about 20% power. Below that, the two-sided
#>   power will be somewhat higher because it counts significant results in
#>   the wrong direction as well. 
#> 
#> -- Reference ----------------------------------------------------------- 
#> 
#>   See Declaration 18.1 in Blair, Coppock, and Humphreys (2023),
#>   "Research Design in the Social Sciences," Princeton University Press,
#>   Chapter 18. 
```
