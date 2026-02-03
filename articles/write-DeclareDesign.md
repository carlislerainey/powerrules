# Computing power with DeclareDesign

[`write_DeclareDesign()`](https://carlislerainey.github.io/powerrules/reference/write_DeclareDesign.md)
generates copy-pasteable DeclareDesign code that reproduces a powerrules
calculation through simulation. The generated code follows Declaration
18.1 in Blair, Coppock, and Humphreys (2023).

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

## Generating the DeclareDesign code

[`write_DeclareDesign()`](https://carlislerainey.github.io/powerrules/reference/write_DeclareDesign.md)
takes the result and prints a self-contained DeclareDesign script:

``` r
write_DeclareDesign(result)
```

    #> -- DeclareDesign Code (Declaration 18.1) ------------------------------- 
    #> 
    #>   library(DeclareDesign)
    #>   
    #>   # Declare a two-arm randomized experiment and diagnose its power
    #>   # via simulation (see Declaration 18.1 in Blair, Coppock, and
    #>   # Humphreys, 2023, Ch. 18).
    #>   
    #>   my_design <-
    #>     # Model: 1000 total respondents (500 per condition).
    #>     # Y = tau * Z + noise, where SD(Y) = 20.8 in the
    #>     # control group (Rule 3).
    #>     declare_model(
    #>       N = 1000,
    #>       U = rnorm(N),
    #>       potential_outcomes(Y ~ 3 * Z + 20.8 * U)
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

## Running the generated code

The generated code runs 500 simulations and reports the simulated power.
Because DeclareDesign uses two-sided tests, the result closely matches
the analytical 74% from powerrules for power levels above about 20%.

``` r
library(DeclareDesign)
```

    #> Loading required package: randomizr

    #> Loading required package: fabricatr

    #> Loading required package: estimatr

``` r
my_design <-
  declare_model(
    N = 1000,
    U = rnorm(N),
    potential_outcomes(Y ~ 3 * Z + 20.8 * U)
  ) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE")

diagnosis <- diagnose_design(my_design, sims = 500,
  diagnosands = declare_diagnosands(
    power = mean(p.value <= 0.10)
  ))
print(diagnosis)
```

    #> 
    #> Research design diagnosis based on 500 simulations. Diagnosis completed in 5 secs. Diagnosand estimates with bootstrapped standard errors in parentheses (100 replicates).
    #> 
    #>     Design Inquiry Estimator Outcome Term N Sims  Power
    #>  my_design     ATE estimator       Y    Z    500   0.72
    #>                                                  (0.02)

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Blair, Graeme, Alexander Coppock, and Macartan Humphreys. 2023.
*Research Design in the Social Sciences: Declaration, Diagnosis, and
Redesign*. Princeton: Princeton University Press.
