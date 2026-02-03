# I have the SD of my outcome. What is my power?

Suppose we know the standard deviation of our outcome in a reference
population, we know how many respondents we can recruit, and we have a
specific treatment effect in mind. We want to find the power—the
probability that the study will produce a statistically significant
result if the true effect equals our assumed value.

[`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md)
piped into
[`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md)
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

The lower bound of Ahler and Sood’s 95% confidence interval is 3 points.
We use this as our assumed effect, tau = 3.

``` r
library(powerrules)

from_sd(sd_y = 20.8) |>
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

The output predicts the standard error from the SD and sample size (Rule
3), then computes power for the assumed effect (Rule 2). With 500 per
condition and no control variables, the study has 74% power to detect a
treatment effect of 3 points. This falls below the conventional 80%
threshold.

## With a pre-post measurement strategy

Clifford, Sheagley, and Piston (2021) suggest measuring the outcome both
before and after treatment and controlling for the pre-treatment
measure. In a small pilot with 250 respondents, Cutler, Pietryka, and
Rainey (2024) measure feelings toward supporters of the out-party before
and after the treatment and find that the pre-treatment measure has an
R² of about 73%. If we set R² = 0.40—a conservative assumption given the
pilot result—the power changes substantially:

``` r
from_sd(sd_y = 20.8, r_squared = 0.40) |>
  find_power(n = 500, tau = 3)
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
    #>     tau   = 3
    #> 
    #>   Predicted SE = 2 * 20.8 * sqrt(1 - 0.4) / sqrt(2 * 500) = 1.02  [Rules 3-4]
    #>   tau / SE     = 3 / 1.02 = 2.94
    #>   Power        = 1 - pnorm(1.64 - 2.94) = 90%                   [Rule 2] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total), assuming a standard deviation of 20.8 and
    #>   control variables that explain 40% of the variance in the outcome, the
    #>   predicted standard error is 1.02. Using a one-sided test at the 0.05
    #>   level, the experiment has 90% power to detect a treatment effect of 3
    #>   units.

The power rises from 74% to 90%. The pre-post adjustment shrinks the
standard error enough to push the study above 80% power for a treatment
effect of 3 points.

## Connection to `find_mde()`

[`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md)
and
[`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md)
are two perspectives on the same calculation. The
`from_sd() |> find_mde()` pipeline with the same inputs shows that the
MDE at 80% power is 3.27 points (without controls). Since tau = 3 is
smaller than 3.27, the power for tau = 3 must be below 80%—and it is
(74%).

## How to choose tau

The `tau` argument is the treatment effect assumed for computing power.
The lower bound of a confidence interval from a closely related study is
a useful starting point (Rainey 2026). In the Ahler and Sood example,
the lower bound of their 95% confidence interval is 3 points, so tau = 3
represents the smallest effect consistent with their findings. Choosing
a tau that is too large makes the study appear more powerful than it is.

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
