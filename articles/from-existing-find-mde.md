# I have an existing study. What effect can I detect?

Suppose we have a published study with a design similar to the one we
are planning. We know the standard error and sample size from that
study, and we know how many respondents we can recruit. We want to find
the minimum detectable effect (MDE)—the smallest treatment effect for
which the study has adequate power (80% or 95%).

[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
piped into
[`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md)
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
and a sample of 268 per condition. Broockman, Kalla, and Westwood (2022)
closely replicate this result, estimating a treatment effect of 3.9 with
a 90% confidence interval of \[1.1, 6.6\].

Suppose we plan to run a similar experiment on a CES module with 1,000
respondents (500 per condition). Using the SE and sample size from Ahler
and Sood, we predict the standard error in our larger study (Rule 7 from
Rainey (2026)).

``` r
library(powerrules)

from_existing(se_existing = 1.8, n_existing = 268) |>
  find_mde(n_planned = 500)
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
    #> 
    #>   Predicted SE = sqrt(268 / 500) * 1.8 = 1.32                   [Rule 7]
    #>   MDE (80% power) = 2.49 * 1.32 = 3.28                          [Rule 5] 
    #>   MDE (95% power) = 3.29 * 1.32 = 4.34                          [Rule 5] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design with 500 respondents per
    #>   condition (1,000 total) replicating an existing study with a standard
    #>   error of 1.8 (268 per condition), the predicted standard error is
    #>   1.32. Using a one-sided test at the 0.05 level, the experiment has 80%
    #>   power to detect a treatment effect of 3.28 units and 95% power to
    #>   detect a treatment effect of 4.34 units. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    #>   95% power. This software uses exact values (2.49 and 3.29), so results
    #>   differ slightly from hand calculations using the rounded factors.

The output scales the existing SE to the planned sample size (Rule 7),
then computes the MDE at 80% and 95% power (Rule 5). With 500 per
condition, the study has 80% power to detect a treatment effect of 3.28
points on the 101-point scale.

## What makes an existing study “close enough”?

The existing study and planned study should share the same design
(balanced, between-subjects), a similar population, and the same or a
similar outcome measure (Rainey 2026). The existing study can differ in
sample size and treatment—the rule adjusts for sample size and the
researcher supplies the assumed effect separately. In the example above,
Ahler and Sood’s study uses the same outcome measure (feeling
thermometer toward out-party supporters) and a similar population (U.S.
adults).

## Where to find SE and n in a published paper

The standard error (SE) is the standard error of the estimated treatment
effect, often reported in regression tables or directly in the text. The
sample size per condition is the number of respondents in each arm, not
the total. If the paper reports only the total sample size for a
balanced design, divide by two.

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

Broockman, David E., Joshua L. Kalla, and Sean J. Westwood. 2022. “Does
Affective Polarization Undermine Democratic Norms or Accountability?
Maybe Not.” *American Journal of Political Science* 67 (3): 808–28.
<https://doi.org/10.1111/ajps.12719>.

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Power (and Automating with Pilot Data).” Center for Open Science.
<https://doi.org/10.31219/osf.io/5am9q_v3>.
