# I have pilot data. What effect can I detect?

Suppose we ran a pilot study and know our planned sample size for the
full study. We want to find the minimum detectable effect (MDE)—the
smallest treatment effect for which the study has adequate power (80% or
95%).

[`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md)
piped into
[`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md)
answers this question.

## Example

Cutler, Pietryka, and Rainey (2024) run a pilot replication of Ahler and
Sood (2018), who find that correcting respondents’ misperceptions of
their out-party reduces affective polarization on a 101-point feeling
thermometer. Ahler and Sood estimate a treatment effect of 6.4 points
with a 95% confidence interval of \[3, 10\]. Cutler et al. use the
pre-post measurement strategy of Clifford, Sheagley, and Piston (2021),
measuring feelings toward supporters of the out-party before and after
treatment. In the pilot, with 85 per condition, they estimate a standard
error of 2.13.

Suppose we plan to run the full study with 500 per condition.

``` r
library(powerrules)

from_pilot(se_pilot = 2.13, n_pilot = 85) |>
  find_mde(n_planned = 500)
```

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

The output predicts the standard error in the full study using the pilot
SE, sample size, and a conservative adjustment factor (Rule 9 from
Rainey (2026)). With 500 per condition, the study has 80% power to
detect a treatment effect of 2.42 points.

## The conservative adjustment factor

Standard errors estimated from small pilot studies are noisy. If the
pilot SE happens to be smaller than the true SE, a naive rescaling would
predict a standard error that is too small, making the study appear more
sensitive than it is (Albers and Lakens 2018; Rainey 2026). The
conservative adjustment factor, sqrt(1/n_(pilot)) + 1, inflates the
predicted SE to protect against this risk. In this example, the
conservative factor is sqrt(1/85) + 1 = 1.11, inflating the predicted SE
by about 11%.

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Albers, Casper, and Daniël Lakens. 2018. “When Power Analyses Based on
Pilot Data Are Biased: Inaccurate Effect Size Estimators and Follow-up
Bias.” *Journal of Experimental Social Psychology* 74 (January): 187–95.
<https://doi.org/10.1016/j.jesp.2017.09.004>.

Clifford, Scott, Geoffrey Sheagley, and Spencer Piston. 2021.
“Increasing Precision Without Altering Treatment Effects: Repeated
Measures Designs in Survey Experiments.” *American Political Science
Review* 115 (3): 1048–65. <https://doi.org/10.1017/s0003055421000241>.

Cutler, Austin Lloyd, Matthew Pietryka, and Carlisle Rainey. 2024.
“Merely Asking: A Replication of Ahler and Sood (2018).”

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Power (and Automating with Pilot Data).” Center for Open Science.
<https://doi.org/10.31219/osf.io/5am9q_v3>.
