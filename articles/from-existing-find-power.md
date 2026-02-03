# I have an existing study. What is my power?

Suppose we have a published study with a design similar to the one we
are planning. We know the standard error and sample size from that
study, we know how many respondents we can recruit, and we have a
specific treatment effect in mind. We want to find the power—the
probability that the study will produce a statistically significant
result if the true effect equals our assumed value.

[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
piped into
[`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md)
answers this question.

## Example

Ahler and Sood (2018) find that correcting respondents’ misperceptions
of their out-party reduces affective polarization. In their experiment,
respondents first report their perceptions of the percent of out-party
members with certain demographic attributes, then receive the correct
information. Compared to a control group, the treatment group evaluated
supporters of the out-party more favorably on a 101-point feeling
thermometer. Ahler and Sood estimate a treatment effect of 6.4 points
with a standard error of 1.8, a 95% confidence interval of \[3, 10\],
and a sample of 268 per condition.

Suppose we plan to run a similar experiment on a CES module with 1,000
respondents (500 per condition). The lower bound of Ahler and Sood’s 95%
confidence interval is 3 points. We use this as our assumed effect, tau
= 3.

``` r
library(powerrules)

from_existing(se_existing = 1.8, n_existing = 268) |>
  find_power(n_planned = 500, tau = 3)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     existing study
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 1.8 
    #>     n (existing)  = 268 per condition
    #>     n (planned)   = 500 per condition (1,000 total)
    #>     tau   = 3
    #> 
    #>   Predicted SE = sqrt(268 / 500) * 1.8 = 1.32                   [Rule 7]
    #>   tau / SE     = 3 / 1.32 = 2.28
    #>   Power        = 1 - pnorm(1.64 - 2.28) = 74%                   [Rule 2] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total) replicating an existing study with a standard
    #>   error of 1.8 (268 per condition), the predicted standard error is
    #>   1.32. Using a one-sided test at the 0.05 level, the experiment has 74%
    #>   power to detect a treatment effect of 3 units.

The output scales the existing SE to the planned sample size (Rule 7),
then computes power for the assumed effect (Rule 2). With 500 per
condition, the study has 74% power to detect a treatment effect of 3
points. This falls below the conventional 80% threshold.

## What makes an existing study “close enough”?

The existing study and planned study should share the same design
(balanced, between-subjects), a similar population, and the same or a
similar outcome measure (Rainey 2026). The existing study can differ in
sample size and treatment—the rule adjusts for sample size and the
researcher supplies the assumed effect separately.

## Interaction designs

For 2×2 factorial designs,
[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
accepts `interaction = TRUE`. The SE computation is unchanged—the
existing SE already encodes the design—but the output reports totals as
N = 4n and labels the design as a 2×2 factorial. See
[`vignette("interactions")`](https://carlislerainey.github.io/powerrules/articles/interactions.md)
for worked examples.

## References

Ahler, Douglas J., and Gaurav Sood. 2018. “The Parties in Our Heads:
Misperceptions about Party Composition and Their Consequences.” *The
Journal of Politics* 80 (3): 964–81. <https://doi.org/10.1086/697253>.

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Power (and Automating with Pilot Data).” Center for Open Science.
<https://doi.org/10.31219/osf.io/5am9q_v3>.
