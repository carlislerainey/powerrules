# I have pilot data. How many respondents do I need?

Suppose we ran a pilot study and have a specific treatment effect in
mind. We want to find the sample size required for the full study to
have adequate power.

[`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md)
piped into
[`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md)
answers this question.

## Example

Cutler, Pietryka, and Rainey (2024) run a pilot replication of Ahler and
Sood (2018), who find that correcting respondents’ misperceptions of
their out-party reduces affective polarization on a 101-point feeling
thermometer. Ahler and Sood estimate a treatment effect of 6.4 points
with a 95% confidence interval of \[3, 10\]. Broockman, Kalla, and
Westwood (2022) closely replicate this result, estimating a treatment
effect of 3.9 with a 90% confidence interval of \[1.1, 6.6\]. Cutler et
al. use the pre-post measurement strategy of Clifford, Sheagley, and
Piston (2021), measuring feelings toward supporters of the out-party
before and after treatment. In the pilot, with 85 per condition, they
estimate a standard error of 2.13.

The lower bound of Ahler and Sood’s 95% confidence interval is 3 points.
We use this as our assumed effect, tau = 3. We want 95% power.

``` r
library(powerrules)

from_pilot(se_pilot = 2.13, n_pilot = 85) |>
  find_n(tau = 3, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     pilot data (conservative)
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (pilot) = 2.13 
    #>     n (pilot)  = 85 per condition
    #>     tau   = 3
    #>     power = 95% 
    #> 
    #>   Conservative factor = sqrt(1 / 85) + 1 = 1.11                 [Rule 9] 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 85 * [3.29 / 3 * 1.11 * 2.13]^2
    #>                       = 570 per condition (1,140 total)        [Rule 10] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design, using pilot data with a
    #>   standard error of 2.13 (85 per condition) and a conservative
    #>   adjustment for pilot noise, the experiment requires 570 respondents
    #>   per condition (1,140 total) for 95% power to detect a treatment effect
    #>   of 3 units, using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The study requires 570 respondents per condition (1,140 total) for 95%
power to detect a treatment effect of 3 points.

## Why the conservative factor matters

The conservative adjustment factor, sqrt(1/n_(pilot)) + 1, inflates the
predicted SE to account for noise in the pilot estimate (Albers and
Lakens 2018; Rainey 2026). This increases the required sample size
compared to treating the pilot as an “existing study.” If we instead
used `from_existing(se_existing = 2.13, n_existing = 85)`, the required
sample size would be smaller, but the resulting study would risk being
underpowered if the pilot SE underestimates the true SE. The
conservative approach protects against this risk by building in a margin
of safety.

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Albers, Casper, and Daniël Lakens. 2018. “When Power Analyses Based on
Pilot Data Are Biased: Inaccurate Effect Size Estimators and Follow-up
Bias.” *Journal of Experimental Social Psychology* 74 (January): 187–95.
<https://doi.org/10.1016/j.jesp.2017.09.004>.

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
