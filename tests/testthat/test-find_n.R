test_that("find_n() from SD computes correct sample size (80% power)", {
  # n = 2 * (mde_factor(0.80) * 20.8 / 3)^2
  mf <- mde_factor(0.80)
  expected <- ceiling(2 * (mf * 20.8 / 3)^2)
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from SD computes correct sample size (95% power)", {
  # n = 2 * (mde_factor(0.95) * 20.8 / 3)^2
  mf <- mde_factor(0.95)
  expected <- ceiling(2 * (mf * 20.8 / 3)^2)
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from SD with adjustment computes correct sample size", {
  # n = 2 * (mde_factor(0.95) * 20.8 * sqrt(1-0.05) / 3)^2
  mf <- mde_factor(0.95)
  expected <- ceiling(2 * (mf * 20.8 * sqrt(1 - 0.05) / 3)^2)
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from existing study computes correct sample size", {
  # n = 268 * (mde_factor(0.95) / 3 * 1.8)^2
  mf <- mde_factor(0.95)
  expected <- ceiling(268 * (mf * 1.8 / 3)^2)
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from pilot computes correct sample size (conservative)", {
  # n = 85 * [mde_factor(0.95) / 3 * (sqrt(1/85) + 1) * 2.13]^2
  mf <- mde_factor(0.95)
  cf <- sqrt(1 / 85) + 1
  expected <- ceiling(85 * (mf * cf * 2.13 / 3)^2)
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from SD (interaction) computes correct sample size", {
  # n = 4 * (mde_factor(0.80) * 2.0 / 0.67)^2
  mf <- mde_factor(0.80)
  expected <- ceiling(4 * (mf * 2.0 / 0.67)^2)
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() from pilot (interaction) computes correct sample size", {
  # n = 75 * [mde_factor(0.80) / 0.67 * (sqrt(1/(2*75)) + 1) * 0.40]^2
  mf <- mde_factor(0.80)
  cf <- sqrt(1 / (2 * 75)) + 1
  expected <- ceiling(75 * (mf * cf * 0.40 / 0.67)^2)
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, expected)
})

test_that("find_n() with 95% CI uses correct MDE factor", {
  mf <- mde_factor(0.80, ci = 0.95)
  expected <- ceiling(2 * (mf * 20.8 / 3)^2)
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80, ci = 0.95)
  expect_equal(res$n_planned, expected)
  expect_equal(res$mde_factor_val, mf, tolerance = 0.001)
})

test_that("find_n() returns ceiling (integer) sample size", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80)
  expect_equal(res$n_planned, ceiling(res$n_planned_raw))
  expect_equal(res$n_planned %% 1, 0)  # is integer
})

test_that("find_n() returns a power_result object invisibly", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "n")
})

test_that("find_n() works with the pipe operator", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  expect_s3_class(res, "power_result")
  expect_true(res$n_planned > 0)
})
