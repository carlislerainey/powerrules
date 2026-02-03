# Verify every worked example from Rainey (2026) "Power Rules" using the
# powerrules pipeline functions. For each example, the paper's sentence(s)
# are quoted as comments, then the pipeline is run and the key numerical
# result is checked.
#
# The paper rounds MDE factors to one decimal place (2.5 for 80% power,
# 3.3 for 95% power with 90% CI). The package uses exact values
# (qnorm(0.95) + qnorm(power)), producing small differences in computed
# results. For example, the paper reports n = 1,050 for Example 2a while
# the pipeline computes n = 1,041. Tolerance of 0.02 (2%) accommodates
# this rounding throughout.

tol <- 0.02

# ===========================================================================
# Example Set 1: Evaluating Power for a Fixed Sample Size
#
# Context: Replicating Ahler & Sood (2018). Correcting respondents'
# misperceptions of their out-party reduces affective polarization on a
# 101-point feeling thermometer. Planned study uses a CES module with
# 1,000 respondents total (500 per condition).
# ===========================================================================

test_that("Example 1a: from SD, no controls, find MDE (n = 500)", {
  # "the standard error in our replication will be about
  #  2 * 20.8 / sqrt(2 * 500) = 1.32. Using Rule 5, a standard error
  #  of 1.32 means that we will have 80% power to detect a treatment
  #  effect of 2.5 * SE = 2.5 * 1.32 = 3.30 points on the 101-point
  #  scale and have 95% power to detect a treatment effect of
  #  3.3 * SE = 3.3 * 1.32 = 4.36 points."
  res <- from_sd(sd_y = 20.8) |> find_mde(n = 500)
  expect_equal(res$se, 1.32, tolerance = tol)
  expect_equal(res$mde_80, 3.30, tolerance = tol)
  expect_equal(res$mde_95, 4.36, tolerance = tol)
})

test_that("Example 1b: from SD, R-squared = 5%, find MDE (n = 500)", {
  # "our standard error will be about
  #  2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500) = 1.28. Using Rule 5,
  #  a standard error of 1.28 means that we will have 80% power to
  #  detect a treatment effect of 2.5 * SE = 2.5 * 1.28 = 3.20 points
  #  on the 101-point scale and have 95% power to detect a treatment
  #  effect of 3.3 * SE = 3.3 * 1.28 = 4.22 points."
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n = 500)
  expect_equal(res$se, 1.28, tolerance = tol)
  expect_equal(res$mde_80, 3.20, tolerance = tol)
  expect_equal(res$mde_95, 4.22, tolerance = tol)
})

test_that("Example 1c: from SD, R-squared = 40%, find MDE (n = 500)", {
  # "the standard error will be about
  #  2 * 20.8 * sqrt(1 - 0.40) / sqrt(2 * 500) = 1.02. For a standard
  #  error of 1.02, we will have 80% power to detect a treatment effect
  #  of 2.5 * SE = 2.5 * 1.02 = 2.55 points on the 101-point scale
  #  and 95% power to detect a treatment effect of
  #  3.3 * SE = 3.3 * 1.02 = 3.37 points."
  res <- from_sd(sd_y = 20.8, r_squared = 0.40) |> find_mde(n = 500)
  expect_equal(res$se, 1.02, tolerance = tol)
  expect_equal(res$mde_80, 2.55, tolerance = tol)
  expect_equal(res$mde_95, 3.37, tolerance = tol)
})

# ===========================================================================
# Example Set 2: Finding the Required Sample Size
#
# Context: Same Ahler & Sood replication. Researcher can choose the
# sample size and wants 95% power to detect a treatment effect of 3 points
# on the 101-point scale (the lower bound of Ahler & Sood's 95% CI).
# ===========================================================================

test_that("Example 2a: from SD, no controls, find n (95% power, tau = 3)", {
  # "we will need about
  #  2 * (3.3 * 20.8 / 3)^2 = 1,050 respondents per condition for
  #  95% power."
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1050, tolerance = tol)
})

test_that("Example 2b: from SD, R-squared = 5%, find n (95% power, tau = 3)", {
  # "With control variables that predict the outcome with an R^2 of 5%,
  #  this becomes
  #  2 * (3.3 * 20.8 * sqrt(1 - 0.05) / 3)^2 = 995."
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 995, tolerance = tol)
})

test_that("Example 2c: from existing (Ahler & Sood), find n (95% power, tau = 3)", {
  # "we will need
  #  268 * (3.3/3 * 1.8)^2 = 1,051 respondents per condition for
  #  95% power to detect a treatment effect of 3 points on the
  #  101-point scale."
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1051, tolerance = tol)
})

test_that("Example 2d: from existing (Broockman et al.), find n (95% power, tau = 3)", {
  # "we will need
  #  502 * (3.3/3 * 1.67)^2 = 1,694 respondents per condition for
  #  95% power to detect a treatment effect of 3 points on the
  #  101-point scale."
  res <- from_existing(se_existing = 1.67, n_existing = 502) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1694, tolerance = tol)
})

test_that("Example 2e: from pilot (Cutler et al.), find n (95% power, tau = 3)", {
  # "they will need (conservatively) about
  #  85 * (3.3/3 * (sqrt(1/85) + 1) * 2.13)^2 = 573 respondents per
  #  condition for 95% power to detect a treatment effect of 3 points
  #  on the 101-point scale."
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 573, tolerance = tol)
})

# ===========================================================================
# Example Set 3: Interactions (Robbins, Pietryka, & Rainey 2024)
#
# Context: 2x2 factorial design (operation succeeds/fails x legislators
# criticize/ignore). Hypothesized interaction of about 0.67 points on a
# seven-point approval scale. SD(Y) = 2.0 (from Myrick 2020). Pilot
# with 75 respondents per condition, SE = 0.40.
# ===========================================================================

test_that("Example 3a: from SD, interaction, find n (80% power, tau = 0.67)", {
  # "the authors would need about
  #  4 * (2.5 * 2.0 / 0.67)^2 = 223 respondents per condition
  #  (or 892 respondents in total) for 80% power to detect an
  #  interaction of 0.67."
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, 223, tolerance = tol)
})

test_that("Example 3b: from SD, interaction, find n (95% power, tau = 0.67)", {
  # "For 95% power, the authors would need about
  #  4 * (3.3 * 2.0 / 0.67)^2 = 389 respondents per condition
  #  (or 1,556 respondents in total)."
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.95)
  expect_equal(res$n_planned, 389, tolerance = tol)
})

test_that("Example 3c: from pilot, interaction, find n (80% power, tau = 0.67)", {
  # "the authors would need about
  #  75 * (2.5/0.67 * (sqrt(1/(2*75)) + 1) * 0.40)^2 = 195
  #  respondents per condition (or 780 respondents total) to have
  #  80% power to detect an interaction of 0.67."
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, 195, tolerance = tol)
})

test_that("Example 3d: from pilot, interaction, find n (95% power, tau = 0.67)", {
  # "For 95% power, they would need about
  #  75 * (3.3/0.67 * (sqrt(1/(2*75)) + 1) * 0.40)^2 = 340
  #  respondents per condition (or 1,360 respondents total) to have
  #  95% power."
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.95)
  expect_equal(res$n_planned, 340, tolerance = tol)
})

test_that("Example 3e: from pilot, interaction, predicted SE (n_planned = 375)", {
  # From the footnote: "the authors would conservatively predict the
  #  standard error would be about
  #  sqrt(75/375) * (sqrt(1/150) + 1) * 0.40 = 0.19.
  #  In the full, 1,500-respondent study, the standard error was 0.18."
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_mde(n_planned = 375)
  expect_equal(res$se, 0.19, tolerance = tol)
})

test_that("Example 3f: from pilot, interaction, power (n_planned = 375, tau = 0.67)", {
  # From the footnote: "the standard error was 0.18, which suggests a
  #  statistical power of about
  #  1 - Phi(1.64 - 0.67/0.18) = 98% to detect an interaction of 0.67."
  #
  # Note: the paper uses the actual SE from the full study (0.18); the
  # pipeline uses the conservative predicted SE from pilot data (~0.19),
  # so the pipeline's power estimate is slightly lower (~97%).
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_power(n_planned = 375, tau = 0.67)
  # Pipeline predicts SE conservatively; power is near but below 98%
  expect_true(res$power > 0.95)
  expect_true(res$power < 0.99)
})

# ===========================================================================
# Example Set 4: Using a 95% Confidence Interval
#
# Context: Same Ahler & Sood setting, but the researcher prefers a 95% CI
# (size-0.025 test). The MDE factor changes from 2.5 to 2.8 for 80%
# power.
# ===========================================================================

test_that("Example 4: from existing, find n (80% power, tau = 3, 95% CI)", {
  # "researchers would need about
  #  268 * (2.8/3 * 1.8)^2 = 756 respondents per condition (note the
  #  2.8 in place of the 2.5; using a 90% confidence interval would
  #  require about 603 respondents per condition)."
  res_95ci <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.80, ci = 0.95)
  expect_equal(res_95ci$n_planned, 756, tolerance = tol)
})

test_that("Example 4 comparison: from existing, find n (80% power, tau = 3, 90% CI)", {
  # The paper notes that "using a 90% confidence interval would require
  #  about 603 respondents per condition."
  res_90ci <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.80, ci = 0.90)
  expect_equal(res_90ci$n_planned, 603, tolerance = tol)
})

