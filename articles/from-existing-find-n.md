# I have an existing study. How many respondents do I need?

Suppose we have a published study with a design similar to the one we
are planning. We know the standard error and sample size from that
study, and we have a specific treatment effect in mind. We want to find
the sample size required for the study to have adequate power.

[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
piped into
[`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md)
answers this question.

## From Ahler and Sood

Ahler and Sood (2018) find that correcting respondents’ misperceptions
of their out-party reduces affective polarization. In their experiment,
respondents first report their perceptions of the percent of out-party
members with certain demographic attributes, then receive the correct
information. Compared to a control group, the treatment group evaluated
supporters of the out-party more favorably on a 101-point feeling
thermometer. Ahler and Sood estimate a treatment effect of 6.4 points
with a standard error of 1.8, a 95% confidence interval of \[3, 10\],
and a sample of 268 per condition.

The lower bound of Ahler and Sood’s 95% confidence interval is 3 points.
We use this as our assumed effect, tau = 3. We want 95% power.

``` r
library(powerrules)

from_existing(se_existing = 1.8, n_existing = 268) |>
  find_n(tau = 3, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     existing study
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 1.8 
    #>     n (existing)  = 268 per condition
    #>     tau   = 3
    #>     power = 95% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 268 * [3.29 / 3 * 1.8]^2
    #>                       = 1,045 per condition (2,090 total)       [Rule 8] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design replicating an existing study
    #>   with a standard error of 1.8 (268 per condition), the experiment
    #>   requires 1,045 respondents per condition (2,090 total) for 95% power
    #>   to detect a treatment effect of 3 units, using a one-sided test at the
    #>   0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The study requires 1,045 respondents per condition (2,090 total) for 95%
power to detect a treatment effect of 3 points.

## From Broockman, Kalla, and Westwood

Broockman, Kalla, and Westwood (2022) closely replicate Ahler and Sood’s
result, estimating a treatment effect of 3.9 with a 90% confidence
interval of \[1.1, 6.6\]. They report a standard error of 1.67 with 502
per condition.

``` r
from_existing(se_existing = 1.67, n_existing = 502) |>
  find_n(tau = 3, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     existing study
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 1.67 
    #>     n (existing)  = 502 per condition
    #>     tau   = 3
    #>     power = 95% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 502 * [3.29 / 3 * 1.67]^2
    #>                       = 1,684 per condition (3,368 total)       [Rule 8] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design replicating an existing study
    #>   with a standard error of 1.67 (502 per condition), the experiment
    #>   requires 1,684 respondents per condition (3,368 total) for 95% power
    #>   to detect a treatment effect of 3 units, using a one-sided test at the
    #>   0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The Broockman et al. estimate implies a larger required sample: 1,684
per condition. The difference arises because Broockman et al. have a
larger sample (502 vs. 268) but a similar SE (1.67 vs. 1.8), suggesting
more variability in their data. Triangulating across multiple existing
studies reveals how sensitive the sample size requirement is to the
choice of source study (Rainey 2026).

## Using a 95% confidence interval

By default, powerrules uses 90% confidence intervals (a size-0.05 test).
If you prefer 95% confidence intervals (a size-0.025 test), set
`ci = 0.95`. The stricter test requires a larger sample.

``` r
from_existing(se_existing = 1.8, n_existing = 268) |>
  find_n(tau = 3, power = 0.80, ci = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     existing study
    #>   CI level:   95% (size-0.025 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 1.8 
    #>     n (existing)  = 268 per condition
    #>     tau   = 3
    #>     power = 80% 
    #> 
    #>   MDE factor          = qnorm(0.975) + qnorm(0.80) = 2.80      [Table 2] 
    #>   n (planned)         = 268 * [2.80 / 3 * 1.8]^2
    #>                       = 758 per condition (1,516 total)         [Rule 8] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design replicating an existing study
    #>   with a standard error of 1.8 (268 per condition), the experiment
    #>   requires 758 respondents per condition (1,516 total) for 80% power to
    #>   detect a treatment effect of 3 units, using a one-sided test at the
    #>   0.025 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.8 for 80% power. This
    #>   software uses the exact value (2.80), so results differ slightly from
    #>   hand calculations using the rounded factor.

With 95% confidence intervals and 80% power, the study requires 758 per
condition—about 27% more than the 597 per condition required with 90%
confidence intervals at the same power level.

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
