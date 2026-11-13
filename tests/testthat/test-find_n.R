# --- Sample size for every source type, interaction off and on ---

test_that("find_n() computes sample size for every source type", {
  mf <- function(power) qnorm(0.95) + qnorm(power)
  cf_pilot     <- sqrt(1 / 85) + 1
  cf_pilot_int <- sqrt(1 / (2 * 75)) + 1

  cases <- list(
    sd_80 = list(
      source = from_sd(sd_y = 20.8), tau = 3, power = 0.80,
      n = 2 * (mf(0.80) * 20.8 / 3)^2),
    sd_95 = list(
      source = from_sd(sd_y = 20.8), tau = 3, power = 0.95,
      n = 2 * (mf(0.95) * 20.8 / 3)^2),
    sd_r2 = list(
      source = from_sd(sd_y = 20.8, r_squared = 0.05), tau = 3, power = 0.95,
      n = 2 * (mf(0.95) * 20.8 * sqrt(1 - 0.05) / 3)^2),
    sd_interaction = list(
      source = from_sd(sd_y = 2.0, interaction = TRUE), tau = 0.67,
      power = 0.80, n = 4 * (mf(0.80) * 2.0 / 0.67)^2),
    existing = list(
      source = from_existing(se_existing = 1.8, n_existing = 268),
      tau = 3, power = 0.95, n = 268 * (mf(0.95) * 1.8 / 3)^2),
    # Pilot Rule 2 is unchanged for interactions
    existing_interaction = list(
      source = from_existing(se_existing = 1.8, n_existing = 268,
                             interaction = TRUE),
      tau = 3, power = 0.95, n = 268 * (mf(0.95) * 1.8 / 3)^2),
    pilot = list(
      source = from_pilot(se_pilot = 2.13, n_pilot = 85), tau = 3,
      power = 0.95, n = 85 * (mf(0.95) * cf_pilot * 2.13 / 3)^2),
    pilot_interaction = list(
      source = from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE),
      tau = 0.67, power = 0.80,
      n = 75 * (mf(0.80) * cf_pilot_int * 0.40 / 0.67)^2)
  )

  for (nm in names(cases)) {
    k <- cases[[nm]]
    res <- find_n(k$source, tau = k$tau, power = k$power)
    expect_equal(res$n_planned, ceiling(k$n), info = nm)
    expect_equal(res$n_planned_raw, k$n, tolerance = 1e-6, info = nm)
  }
})

# --- Returned object ---

test_that("find_n() returns a power_result with the expected fields", {
  s <- from_sd(sd_y = 20.8)
  res <- find_n(s, tau = 3)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "n")
  expect_identical(res$source, s)
  expect_equal(res$ci, 0.90)
  expect_equal(res$tau, 3)
  expect_equal(res$power, 0.80)  # default
  expect_equal(res$mde_factor_val, 2.4863, tolerance = 0.001)
  # SE is evaluated at the (rounded-up) planned sample size
  expect_equal(res$se, 2 * 20.8 / sqrt(2 * res$n_planned), tolerance = 0.001)
})

test_that("find_n() returns the ceiling (integer) sample size", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80)
  expect_gt(res$n_planned_raw %% 1, 0)  # the raw value is not a whole number
  expect_equal(res$n_planned, ceiling(res$n_planned_raw))
  expect_equal(res$n_planned %% 1, 0)  # is integer
})

# --- power argument and 95% CI ---

test_that("find_n() uses the requested power level", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_equal(res$power, 0.95)
  expect_equal(res$mde_factor_val, 3.2897, tolerance = 0.001)
})

test_that("find_n() with 95% CI uses correct MDE factor", {
  mf <- qnorm(0.975) + qnorm(0.80)
  expected <- ceiling(2 * (mf * 20.8 / 3)^2)
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80, ci = 0.95)
  expect_equal(res$n_planned, expected)
  expect_equal(res$ci, 0.95)
  expect_equal(res$mde_factor_val, mf, tolerance = 0.001)
  expect_equal(res$mde_factor_val, mde_factor(0.80, ci = 0.95),
               tolerance = 0.001)
})
