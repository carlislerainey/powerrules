# Interactions in factorial designs

Detecting interactions in 2×2 factorial designs requires substantially
more power than detecting treatment effects in two-group designs. This
vignette explains why and shows how to use `interaction = TRUE` in the
powerrules pipeline.

## Why interactions require more power

Rainey (2026) identifies three reasons interactions typically need
larger samples than main effects. First, estimating an interaction
requires comparing four group means rather than two, which increases the
standard error by a factor of 2 compared to a two-group treatment effect
with the same total sample size. Second, a 2×2 factorial design has four
conditions rather than two, so the same total sample size means fewer
respondents per condition. Third, interactions are often smaller than
main effects. Together, these factors can require as much as 16 times
the total sample size to achieve the same power for an interaction as
for a main effect with similar precision—Gelman’s “16x” intuition.

## Finding the required sample size from SD

Robbins et al. (2024) study how respondents evaluate in-party
legislators who criticize an out-party president following a covert
operation. They use a 2×2 factorial design: the operation succeeds or
fails, and legislators either criticize or do not criticize. They
hypothesize an interaction of about 0.67 points on a seven-point
approval scale. Myrick (2020) studies a related outcome using a similar
population. The standard deviation of the approval scale in Myrick’s
data is about 2.0.

``` r
library(powerrules)

from_sd(sd_y = 2.0, interaction = TRUE) |>
  find_n(tau = 0.67, power = 0.80)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 2 
    #>     tau   = 0.67 (interaction)
    #>     power = 80% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.80) = 2.49       [Table 2] 
    #>   n (planned)         = 4 * (2.49 * 2 / 0.67)^2
    #>                       = 221 per condition (884 total, 4 conditions)  [Rule 6*] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design, assuming a standard deviation of
    #>   2, the experiment requires 221 respondents per condition (884 total, 4
    #>   conditions) for 80% power to detect an interaction of 0.67 units,
    #>   using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power. This
    #>   software uses the exact value (2.49), so results differ slightly from
    #>   hand calculations using the rounded factor.

The study requires 221 per condition (884 total across four conditions)
for 80% power to detect an interaction of 0.67 on the seven-point scale.
For 95% power:

``` r
from_sd(sd_y = 2.0, interaction = TRUE) |>
  find_n(tau = 0.67, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 2 
    #>     tau   = 0.67 (interaction)
    #>     power = 95% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 4 * (3.29 * 2 / 0.67)^2
    #>                       = 386 per condition (1,544 total, 4 conditions)  [Rule 6*] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design, assuming a standard deviation of
    #>   2, the experiment requires 386 respondents per condition (1,544 total,
    #>   4 conditions) for 95% power to detect an interaction of 0.67 units,
    #>   using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The study requires 386 per condition (1,544 total) for 95% power.

## Finding the required sample size from pilot data

Robbins et al. ran a pilot study with 75 per condition. The standard
error of the interaction estimate is 0.40.

``` r
from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
  find_n(tau = 0.67, power = 0.80)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     pilot data (conservative)
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (pilot) = 0.4 
    #>     n (pilot)  = 75 per condition
    #>     tau   = 0.67 (interaction)
    #>     power = 80% 
    #> 
    #>   Conservative factor = sqrt(1 / (2 * 75)) + 1 = 1.08          [Rule 9*] 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.80) = 2.49       [Table 2] 
    #>   n (planned)         = 75 * [2.49 / 0.67 * 1.08 * 0.4]^2
    #>                       = 194 per condition (776 total, 4 conditions)  [Rule 10*] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design, using pilot data with a standard
    #>   error of 0.4 (75 per condition) and a conservative adjustment for
    #>   pilot noise, the experiment requires 194 respondents per condition
    #>   (776 total, 4 conditions) for 80% power to detect an interaction of
    #>   0.67 units, using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power. This
    #>   software uses the exact value (2.49), so results differ slightly from
    #>   hand calculations using the rounded factor.

``` r
from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
  find_n(tau = 0.67, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     pilot data (conservative)
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (pilot) = 0.4 
    #>     n (pilot)  = 75 per condition
    #>     tau   = 0.67 (interaction)
    #>     power = 95% 
    #> 
    #>   Conservative factor = sqrt(1 / (2 * 75)) + 1 = 1.08          [Rule 9*] 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 75 * [3.29 / 0.67 * 1.08 * 0.4]^2
    #>                       = 339 per condition (1,356 total, 4 conditions)  [Rule 10*] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design, using pilot data with a standard
    #>   error of 0.4 (75 per condition) and a conservative adjustment for
    #>   pilot noise, the experiment requires 339 respondents per condition
    #>   (1,356 total, 4 conditions) for 95% power to detect an interaction of
    #>   0.67 units, using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The pilot data yield smaller required sample sizes (194 and 339 per
condition, compared to 221 and 386 from the SD). The pilot SE reflects
the actual design, including any control variables, so it can be more
informative than the SD alone.

## Predicting SE and power for a planned sample size

Robbins et al. ultimately ran the full study with 375 per condition. We
can predict the standard error and compute the MDE:

``` r
from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
  find_mde(n_planned = 375)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     pilot data (conservative)
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (pilot) = 0.4 
    #>     n (pilot)  = 75 per condition
    #>     n (planned)   = 375 per condition (1,500 total)
    #> 
    #>   Predicted SE = sqrt(75 / 375) * (sqrt(1/(2*75)) + 1) * 0.4 = 0.19  [Rule 9*]
    #>   MDE (80% power) = 2.49 * 0.19 = 0.48                          [Rule 5] 
    #>   MDE (95% power) = 3.29 * 0.19 = 0.64                          [Rule 5] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design with 375 respondents per
    #>   condition (1,500 total, 4 conditions), using pilot data with a
    #>   standard error of 0.4 (75 per condition) and a conservative adjustment
    #>   for pilot noise, the predicted standard error is 0.19. Using a
    #>   one-sided test at the 0.05 level, the experiment has 80% power to
    #>   detect an interaction of 0.48 units and 95% power to detect an
    #>   interaction of 0.64 units. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    #>   95% power. This software uses exact values (2.49 and 3.29), so results
    #>   differ slightly from hand calculations using the rounded factors.

And compute the power for the hypothesized interaction of 0.67:

``` r
from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
  find_power(n_planned = 375, tau = 0.67)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     pilot data (conservative)
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (pilot) = 0.4 
    #>     n (pilot)  = 75 per condition
    #>     n (planned)   = 375 per condition (1,500 total)
    #>     tau   = 0.67 (interaction)
    #> 
    #>   Predicted SE = sqrt(75 / 375) * (sqrt(1/(2*75)) + 1) * 0.4 = 0.19  [Rule 9*]
    #>   tau / SE     = 0.67 / 0.19 = 3.46
    #>   Power        = 1 - pnorm(1.64 - 3.46) = 97%                   [Rule 2] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design with 375 respondents per
    #>   condition (1,500 total, 4 conditions), using pilot data with a
    #>   standard error of 0.4 (75 per condition) and a conservative adjustment
    #>   for pilot noise, the predicted standard error is 0.19. Using a
    #>   one-sided test at the 0.05 level, the experiment has 97% power to
    #>   detect an interaction of 0.67 units.

The conservative predicted SE is 0.19, and the study has 97% power for
an interaction of 0.67. The actual SE in the full study turned out to be
0.18, close to the conservative prediction.

## Using `from_existing()` for interactions

The
[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
function accepts `interaction = TRUE`. Unlike
[`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md)
and
[`from_pilot()`](https://carlislerainey.github.io/powerrules/reference/from_pilot.md),
the `interaction` parameter does not change the SE computation. The SE
from an existing study already reflects the interaction structure of
that study—if the existing study estimated an interaction, its SE
already incorporates the larger variance associated with comparing four
groups. The `interaction` parameter controls only the display: totals
use N = 4n (four conditions) instead of N = 2n, and the design label and
manuscript sentence reflect the factorial structure.

Suppose Robbins et al. ran their full study with 375 per condition and
obtained a standard error of 0.18 for the interaction. We can treat this
as an “existing study” and use
[`from_existing()`](https://carlislerainey.github.io/powerrules/reference/from_existing.md)
to plan a replication:

``` r
from_existing(se_existing = 0.18, n_existing = 375, interaction = TRUE) |>
  find_mde(n_planned = 500)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     existing study
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 0.18 
    #>     n (existing)  = 375 per condition
    #>     n (planned)   = 500 per condition (2,000 total)
    #> 
    #>   Predicted SE = sqrt(375 / 500) * 0.18 = 0.16                  [Rule 7]
    #>   MDE (80% power) = 2.49 * 0.16 = 0.39                          [Rule 5] 
    #>   MDE (95% power) = 3.29 * 0.16 = 0.51                          [Rule 5] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design with 500 respondents per
    #>   condition (2,000 total, 4 conditions) replicating an existing study
    #>   with a standard error of 0.18 (375 per condition), the predicted
    #>   standard error is 0.16. Using a one-sided test at the 0.05 level, the
    #>   experiment has 80% power to detect an interaction of 0.39 units and
    #>   95% power to detect an interaction of 0.51 units. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    #>   95% power. This software uses exact values (2.49 and 3.29), so results
    #>   differ slightly from hand calculations using the rounded factors.

The SE is scaled from the existing study to the planned sample size
using Rule 7, exactly as for a two-group design. The output reports the
total as 2,000 (4 × 500) because `interaction = TRUE` tells the pipeline
that the planned study is a 2×2 factorial.

``` r
from_existing(se_existing = 0.18, n_existing = 375, interaction = TRUE) |>
  find_n(tau = 0.67, power = 0.80)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, 2x2 factorial (interaction)
    #>   Source:     existing study
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SE (existing) = 0.18 
    #>     n (existing)  = 375 per condition
    #>     tau   = 0.67 (interaction)
    #>     power = 80% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.80) = 2.49       [Table 2] 
    #>   n (planned)         = 375 * [2.49 / 0.67 * 0.18]^2
    #>                       = 168 per condition (672 total, 4 conditions)  [Rule 8*] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, 2x2 factorial design replicating an existing study
    #>   with a standard error of 0.18 (375 per condition), the experiment
    #>   requires 168 respondents per condition (672 total, 4 conditions) for
    #>   80% power to detect an interaction of 0.67 units, using a one-sided
    #>   test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 2.5 for 80% power. This
    #>   software uses the exact value (2.49), so results differ slightly from
    #>   hand calculations using the rounded factor.

## References

Myrick, Rachel. 2020. “Why so Secretive? Unpacking Public Attitudes
Toward Secrecy and Success in US Foreign Policy.” *The Journal of
Politics* 82 (3): 828–43. <https://doi.org/10.1086/707308>.

Rainey, Carlisle. 2026. “Power Rules: Practical Advice for Computing
Power (and Automating with Pilot Data).” Center for Open Science.
<https://doi.org/10.31219/osf.io/5am9q_v3>.

Robbins, Caroline, Alessandro Brunelli, Jose Casto, Ainsley Coty, Andrew
Louis, Bryanna Major, Maria A Martinez, et al. 2024. “Overt Consequences
of Covert Actions: Success, Failure, and Voters’ Preferences for
Legislative Oversight.” SocArXiv.
<https://doi.org/10.31235/osf.io/9p5h8>.
