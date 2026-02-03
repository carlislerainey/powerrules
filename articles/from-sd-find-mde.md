# I have the SD of my outcome. What effect can I detect?

Suppose we know the standard deviation of our outcome in a reference
population and we know how many respondents we can recruit. We want to
find the minimum detectable effect (MDE)—the smallest treatment effect
for which the study has adequate power (80% or 95%).

[`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md)
piped into
[`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md)
answers this question.

## Without control variables

Imagine we are planning to replicate Ahler and Sood (2018), who find
that correcting respondents’ misperceptions of their out-party reduces
affective polarization. In their experiment, respondents first report
their perceptions of the percent of out-party members with certain
demographic attributes, then receive the correct information. Compared
to a control group, the treatment group evaluated supporters of the
out-party more favorably on a 101-point feeling thermometer. Ahler and
Sood estimate a treatment effect of 6.4 points with a 95% confidence
interval of \[3, 10\]. Broockman, Kalla, and Westwood (2022) closely
replicate this result, estimating a treatment effect of 3.9 with a 90%
confidence interval of \[1.1, 6.6\].

Suppose we plan to run this experiment on a CES module with 1,000
respondents (500 per condition). To predict the standard error using
Rule 3 of Rainey (2026), we need the standard deviation of the outcome
in a reference population. The 2020 American National Elections Study
(ANES) asks a similar question: respondents report their feelings toward
the Democratic or Republican *party* on a 101-point feeling thermometer.
Ahler and Sood ask about *supporters of* the party rather than the party
itself, so the measures are not identical, but the ANES provides a
reasonable approximation. The standard deviation of the ANES feeling
thermometer responses is 20.8.

``` r
library(powerrules)

from_sd(sd_y = 20.8) |>
  find_mde(n = 500)
```

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

The output predicts the standard error from the SD and sample size (Rule
3), then computes the MDE at 80% and 95% power by multiplying the
predicted SE by the MDE factor (Rule 5). It also provides a
manuscript-ready sentence.

With 500 per condition and no control variables, the study has 80% power
to detect a treatment effect of 3.27 points on the 101-point scale. If
we judge that effects smaller than 3.27 points are not substantively
meaningful, the study has adequate power.

## With control variables

Regression adjustment for control variables that predict the outcome
shrinks the standard error (Rule 4 from Rainey 2026). The `r_squared`
argument specifies the R² of a regression of the outcome on the planned
control variables in a reference population.

Broockman, Kalla, and Westwood (2022) control for a seven-point party
identification scale and partisan strength. In the 2020 ANES, these two
variables have an R² of 5% for the feeling thermometer toward the
Democratic and Republican parties (rather than “supporters of” those
parties). For R² = 5%, regression adjustment shrinks the standard error
by about 2.5%.

``` r
from_sd(sd_y = 20.8, r_squared = 0.05) |>
  find_mde(n = 500)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 20.8 
    #>     R^2   = 0.05 
    #>     n     = 500 per condition (1,000 total)
    #> 
    #>   Predicted SE = 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500) = 1.28  [Rules 3-4]
    #>   MDE (80% power) = 2.49 * 1.28 = 3.19                          [Rule 5] 
    #>   MDE (95% power) = 3.29 * 1.28 = 4.22                          [Rule 5] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total), assuming a standard deviation of 20.8 and
    #>   control variables that explain 5% of the variance in the outcome, the
    #>   predicted standard error is 1.28. Using a one-sided test at the 0.05
    #>   level, the experiment has 80% power to detect a treatment effect of
    #>   3.19 units and 95% power to detect a treatment effect of 4.22 units. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    #>   95% power. This software uses exact values (2.49 and 3.29), so results
    #>   differ slightly from hand calculations using the rounded factors.

The MDE at 80% power drops from 3.27 to 3.19. These controls shrink the
standard error, but not substantially.

## With a pre-post measurement strategy

Clifford, Sheagley, and Piston (2021) suggest measuring the outcome both
before and after treatment and controlling for the pre-treatment
measure. The pre-treatment measure should be *strongly* related to the
post-treatment outcome. In a small pilot with 250 respondents, Cutler,
Pietryka, and Rainey (2024) measure feelings toward supporters of the
out-party before and after the treatment and find that the pre-treatment
measure has an R² of about 73%.

If we set R² = 0.40—a conservative assumption given the pilot result,
though additional work should confirm this assumption—the MDE changes
substantially:

``` r
from_sd(sd_y = 20.8, r_squared = 0.40) |>
  find_mde(n = 500)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 20.8 
    #>     R^2   = 0.4 
    #>     n     = 500 per condition (1,000 total)
    #> 
    #>   Predicted SE = 2 * 20.8 * sqrt(1 - 0.4) / sqrt(2 * 500) = 1.02  [Rules 3-4]
    #>   MDE (80% power) = 2.49 * 1.02 = 2.53                          [Rule 5] 
    #>   MDE (95% power) = 3.29 * 1.02 = 3.35                          [Rule 5] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total), assuming a standard deviation of 20.8 and
    #>   control variables that explain 40% of the variance in the outcome, the
    #>   predicted standard error is 1.02. Using a one-sided test at the 0.05
    #>   level, the experiment has 80% power to detect a treatment effect of
    #>   2.53 units and 95% power to detect a treatment effect of 3.35 units. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    #>   95% power. This software uses exact values (2.49 and 3.29), so results
    #>   differ slightly from hand calculations using the rounded factors.

The MDE at 80% power drops to 2.53 points, a 23% reduction compared to
no controls, without recruiting a single additional respondent. This
pre-post adjustment strategy can determine whether a 1,000-respondent
study has adequate power.

## Where to find SD(Y)

SD(Y) should come from a reference population that measures the same (or
a similar) outcome on a similar population (Rainey 2026). Good sources
include large surveys (e.g., ANES, CES, GSS) and descriptive statistics
reported in published papers. In the example above, the ANES measures
feelings toward the *party* while Ahler and Sood measure feelings toward
*supporters of* the party—not identical, but a reasonable approximation.

## How to choose R²

The `r_squared` argument is the R² of a regression of the outcome on the
planned control variables in a reference population. Overestimating R²
produces an MDE that is too small, making the study appear more
sensitive than it is.

- **R² = 0** (the default) assumes no control variables.
- **R² of a few percent** is typical for standard demographics (age,
  gender, partisanship), as in the example above.
- **R² = 0.30 to 0.50** is plausible for pre-post designs (Clifford,
  Sheagley, and Piston 2021).

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Broockman, David E., Joshua L. Kalla, and Sean J. Westwood. 2022. “Does
Affective Polarization Undermine Democratic Norms or Accountability?
Maybe Not.” *American Journal of Political Science* 67 (3): 808–28.
<https://doi.org/10.1111/ajps.12719>.

Clifford, Scott, Geoffrey Sheagley, and Spencer Piston. 2021.
“Increasing Precision Without Altering Treatment Effects: Repeated
Measures Designs in Survey Experiments.” *American Political Science
Review* 115 (3): 1048–65. <https://doi.org/10.1017/s0003055421000241>.

Cutler, Austin Lloyd, Matthew Pietryka, and Carlisle Rainey. 2024.
“Merely Asking: A Replication of Ahler and Sood (2018).”

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Power (and Automating with Pilot Data).” Center for Open Science.
<https://doi.org/10.31219/osf.io/5am9q_v3>.
