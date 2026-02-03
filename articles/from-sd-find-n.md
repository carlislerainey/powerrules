# I have the SD of my outcome. How many respondents do I need?

Suppose we know the standard deviation of our outcome in a reference
population and we have a specific treatment effect in mind. We want to
find the sample size required for the study to have adequate power.

[`from_sd()`](https://carlislerainey.github.io/powerrules/reference/from_sd.md)
piped into
[`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md)
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

To predict the standard error using Rule 3 of Rainey (2026), we need the
standard deviation of the outcome in a reference population. The 2020
American National Elections Study (ANES) asks a similar question:
respondents report their feelings toward the Democratic or Republican
*party* on a 101-point feeling thermometer. Ahler and Sood ask about
*supporters of* the party rather than the party itself, so the measures
are not identical, but the ANES provides a reasonable approximation. The
standard deviation of the ANES feeling thermometer responses is 20.8.

The lower bound of Ahler and Sood’s 95% confidence interval is 3 points.
We use this as our assumed effect, tau = 3. We want 95% power.

``` r
library(powerrules)

from_sd(sd_y = 20.8) |>
  find_n(tau = 3, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 20.8 
    #>     tau   = 3
    #>     power = 95% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 2 * (3.29 * 20.8 / 3)^2
    #>                       = 1,041 per condition (2,082 total)       [Rule 6] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design, assuming a standard deviation
    #>   of 20.8, the experiment requires 1,041 respondents per condition
    #>   (2,082 total) for 95% power to detect a treatment effect of 3 units,
    #>   using a one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

The output computes the MDE factor for the requested power level, then
solves for the sample size (Rule 6). The study requires 1,041
respondents per condition (2,082 total) for 95% power to detect a
treatment effect of 3 points.

## With control variables

Broockman, Kalla, and Westwood (2022) control for a seven-point party
identification scale and partisan strength. In the 2020 ANES, these two
variables have an R² of 5% for the feeling thermometer toward the
Democratic and Republican parties (rather than “supporters of” those
parties). For R² = 5%, regression adjustment shrinks the standard error
by about 2.5%.

``` r
from_sd(sd_y = 20.8, r_squared = 0.05) |>
  find_n(tau = 3, power = 0.95)
```

    #> -- Power Analysis ------------------------------------------------------ 
    #>   Design:     balanced, between-subjects
    #>   Source:     reference population SD
    #>   CI level:   90% (size-0.05 test of directional hypothesis)
    #> 
    #>   Inputs:
    #>     SD(Y) = 20.8 
    #>     R^2   = 0.05 
    #>     tau   = 3
    #>     power = 95% 
    #> 
    #>   MDE factor          = qnorm(0.95) + qnorm(0.95) = 3.29       [Table 2] 
    #>   n (planned)         = 2 * (3.29 * 20.8 * sqrt(1 - 0.05) / 3)^2
    #>                       = 989 per condition (1,978 total)         [Rule 6] 
    #> 
    #> -- Manuscript sentence (edit as needed) -------------------------------- 
    #>   For a balanced, between-subjects design, assuming a standard deviation
    #>   of 20.8 and control variables that explain 5% of the variance in the
    #>   outcome, the experiment requires 989 respondents per condition (1,978
    #>   total) for 95% power to detect a treatment effect of 3 units, using a
    #>   one-sided test at the 0.05 level. 
    #> 
    #>   Note: The paper rounds the MDE factor to 3.3 for 95% power. This
    #>   software uses the exact value (3.29), so results differ slightly from
    #>   hand calculations using the rounded factor.

With control variables, the required sample drops from 1,041 to 989 per
condition. These controls help, but modestly.

## How to choose tau and power

The `tau` argument is the treatment effect assumed for computing power.
The lower bound of a confidence interval from a closely related study is
a useful starting point (Rainey 2026). In the Ahler and Sood example,
the lower bound of their 95% confidence interval is 3 points.

The `power` argument (default 0.80) sets the desired power level. Higher
power requires more respondents. Rainey (2026) recommends 80% power as a
baseline and 95% power as a stretch goal. Both pipelines above use 95%
power; at 80% power (the default), the required sample sizes would be
smaller.

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
