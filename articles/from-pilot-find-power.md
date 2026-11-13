# I have pilot data. What is my power?

Suppose we ran a pilot study and know our planned sample size for the
full study. We have a specific treatment effect in mind. We want to find
the power—the probability that the study will produce a statistically
significant result if the true effect equals our assumed value.

[`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md)
piped into
[`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md)
answers this question.

## Example

Cutler et al. (2026) run a pilot replication of Ahler and Sood (2018),
who find that correcting respondents’ misperceptions of their out-party
reduces affective polarization on a 101-point feeling thermometer. Ahler
and Sood estimate a treatment effect of 6.4 points with a 95% confidence
interval of \[3, 10\]. Cutler et al. use the pre-post measurement
strategy of Clifford et al. (2021), measuring feelings toward supporters
of the out-party before and after treatment. In the pilot, with 85 per
condition, they estimate a standard error of 2.13.

Suppose we plan to run the full study with 500 per condition. The lower
bound of Ahler and Sood’s 95% confidence interval is 3 points. We use
this as our assumed effect, tau = 3.

``` r

library(powerrules)

from_pilot(se_pilot = 2.13, n_pilot = 85) |>
  find_power(n_planned = 500, tau = 3)
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
    #>     tau   = 3
    #> 
    #>   Predicted SE = sqrt(85 / 500) * (sqrt(1/85) + 1) * 2.13 = 0.97  [Pilot Rule 3]
    #>   tau / SE     = 3 / 0.97 = 3.08
    #>   Power        = 1 - pnorm(1.64 - 3.08) = 92%                   [Rule 2] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total), using pilot data with a standard error of
    #>   2.13 (85 per condition) and a conservative adjustment for pilot noise,
    #>   the predicted standard error is 0.97. Using a one-sided test at the
    #>   0.05 level, the experiment has 92% power to detect a treatment effect
    #>   of 3 units.

The output predicts the standard error using the pilot SE, sample size,
and a conservative adjustment factor (Pilot Rule 3, which is Rule 3 from
Rainey (2026a)), then computes power for the assumed effect (Rule 2 from
Rainey (2026b)). With 500 per condition, the study has 92% power to
detect a treatment effect of 3 points.

## The conservative adjustment factor

Standard errors estimated from small pilot studies are noisy. If the
pilot SE happens to be smaller than the true SE, a naive rescaling would
predict a standard error that is too small, overstating the study’s
power (Albers and Lakens 2018; Rainey 2026a). The conservative
adjustment factor inflates the predicted SE to protect against this
risk. Despite the conservative adjustment, the study still has 92% power
for tau = 3, suggesting the planned sample size is adequate.

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *Journal
of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Albers, Casper, and Daniël Lakens. 2018. “When Power Analyses Based on
Pilot Data Are Biased: Inaccurate Effect Size Estimators and Follow-up
Bias.” *Journal of Experimental Social Psychology* 74: 187–95.
<https://doi.org/10.1016/j.jesp.2017.09.004>.

Clifford, Scott, Geoffrey Sheagley, and Spencer Piston. 2021.
“Increasing Precision Without Altering Treatment Effects: Repeated
Measures Designs in Survey Experiments.” *American Political Science
Review* 115 (3): 1048–65. <https://doi.org/10.1017/S0003055421000241>.

Cutler, Austin Lloyd, Matthew T. Pietryka, and Carlisle Rainey. 2026.
“Merely Asking: A Replication of Ahler and Sood (2018).” Unpublished
manuscript.

Rainey, Carlisle. 2026a. “Automating Power Analysis with Pilot Data.”
Unpublished manuscript.

Rainey, Carlisle. 2026b. “Power Rules: Practical Advice for Computing
Statistical Power.” *Political Analysis*.
<https://osf.io/preprints/osf/5am9q>.
