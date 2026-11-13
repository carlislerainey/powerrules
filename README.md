
# powerrules <a href="https://carlislerainey.github.io/powerrules/"><img src="man/figures/logo.png" align="right" height="139" alt="powerrules website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/carlislerainey/powerrules/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlislerainey/powerrules/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Power analysis for balanced, between-subjects designs using the rules
from Rainey (2026) “Power Rules: Practical Advice for Computing
Statistical Power” and its companion paper, “Automating Power Analysis
with Pilot Data.” The [preprint](https://osf.io/preprints/osf/5am9q) is
on OSF.

## Quick Start

The main function is `power_rules()`, which interactively walks you
through the logic presented in the paper, step by step. At the end, it
gives you a one-line `from_*() |> find_*()` pipeline that mirrors the
conceptual logic from the paper. You can save this pipeline as a record
of your inputs and outputs.

``` r
# install from github
pak::pak("carlislerainey/powerrules")

# run the interactive guide
powerrules::power_rules()
```

## Overview

Rainey (2026) argues for the value of simple rules of thumb that let you
predict statistical power with paper, a pencil, and a pocket calculator.
Predicting power involves guesswork, and paper-and-pencil calculations
better match the spirit of the task than specialized software that
brings an air of false exactness. Pencil-and-paper work also constantly
reminds you how inputs relate to outputs, which matters when you are
*designing* an experiment and have freedom to make different choices.

powerrules automates these rules while preserving their transparency.
Every computation shows its work, so you can see exactly how inputs
relate to outputs. The package tries to maintain the feel of
paper-and-pencil.

## Using the Package

**`power_rules()`** is the primary entry point. It asks about your
design, collects the numbers it needs, shows the computation with rule
citations, and gives you a copyable `from_*() |> find_*()` pipeline. The
pipeline is the permanent artifact you save to your script.

**`shiny_rules()`** is a browser-based alternative. It performs the same
computation, presented as a fill-in-the-blank board, and also outputs
the pipeline.

Both guides produce a one-line pipeline in the form
`from_*() |> find_*()`. Here is an example:

``` r
library(powerrules)

from_sd(sd_y = 20.8) |>
  find_mde(n = 500)
```

    ## -- Power Analysis ------------------------------------------------------ 
    ##   Design:     balanced, between-subjects
    ##   Source:     reference population SD
    ##   CI level:   90% (size-0.05 test of directional hypothesis)
    ## 
    ##   Inputs:
    ##     SD(Y) = 20.8 
    ##     n     = 500 per condition (1,000 total)
    ## 
    ##   Predicted SE = 2 * 20.8 / sqrt(2 * 500) = 1.32                [Rule 3]
    ##   MDE (80% power) = 2.49 * 1.32 = 3.27                          [Rule 5] 
    ##   MDE (95% power) = 3.29 * 1.32 = 4.33                          [Rule 5] 
    ## 
    ## -- Manuscript sentence (edit as needed) -------------------------------- 
    ##   For a balanced, between-subjects design with 500 respondents per
    ##   condition (1,000 total), assuming a standard deviation of 20.8, the
    ##   predicted standard error is 1.32. Using a one-sided test at the 0.05
    ##   level, the experiment has 80% power to detect a treatment effect of
    ##   3.27 units and 95% power to detect a treatment effect of 4.33 units. 
    ## 
    ##   Note: The paper rounds the MDE factor to 2.5 for 80% power and 3.3 for
    ##   95% power. This software uses exact values (2.49 and 3.29), so results
    ##   differ slightly from hand calculations using the rounded factors.

The output shows the inputs, step-by-step formulas with rule references,
the MDE at 80% and 95% power, and a manuscript-ready sentence you can
copy into a pre-registration or methods section.

## Vignettes

Each find-from combination has a dedicated vignette with a fully worked
example.

|                        | Find the MDE                                                                                 | Find the power                                                                                 | Find the sample size                                                                       |
|:-----------------------|:---------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------|
| From a reference SD    | [vignette](https://carlislerainey.github.io/powerrules/articles/from-sd-find-mde.html)       | [vignette](https://carlislerainey.github.io/powerrules/articles/from-sd-find-power.html)       | [vignette](https://carlislerainey.github.io/powerrules/articles/from-sd-find-n.html)       |
| From an existing study | [vignette](https://carlislerainey.github.io/powerrules/articles/from-existing-find-mde.html) | [vignette](https://carlislerainey.github.io/powerrules/articles/from-existing-find-power.html) | [vignette](https://carlislerainey.github.io/powerrules/articles/from-existing-find-n.html) |
| From pilot data        | [vignette](https://carlislerainey.github.io/powerrules/articles/from-pilot-find-mde.html)    | [vignette](https://carlislerainey.github.io/powerrules/articles/from-pilot-find-power.html)    | [vignette](https://carlislerainey.github.io/powerrules/articles/from-pilot-find-n.html)    |

Additional vignettes:

- [Interactions](https://carlislerainey.github.io/powerrules/articles/interactions.html)
  — 2x2 factorial designs

## Verify Your Results

powerrules can generate standalone code to verify any calculation three
ways:

- `write_manual()` generates base R code using the paper’s formulas.
  \[[vignette](https://carlislerainey.github.io/powerrules/articles/write-manual.html)\]
- `write_DeclareDesign()` generates a DeclareDesign simulation.
  \[[vignette](https://carlislerainey.github.io/powerrules/articles/write-DeclareDesign.html)\]
- `write_loop()` generates a for-loop simulation.
  \[[vignette](https://carlislerainey.github.io/powerrules/articles/write-loop.html)\]

Each function takes a pipeline result and prints copy-pasteable code
that reproduces the `from_*() |> find_*()` result with an alternative
approach.

## Citation

Rainey, Carlisle (2026). “Power Rules: Practical Advice for Computing
Statistical Power.” *Political Analysis*. Forthcoming. Preprint:
<https://osf.io/preprints/osf/5am9q>.

BibTeX:

``` bibtex
@article{Rainey_2026,
  title = {Power Rules: Practical Advice for Computing Statistical Power},
  author = {Rainey, Carlisle},
  journal = {Political Analysis},
  year = {2026},
  note = {Forthcoming},
  url = {https://osf.io/preprints/osf/5am9q}
}
```

## License

MIT
