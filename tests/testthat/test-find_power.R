# --- Power for every source type, interaction off and on ---

test_that("find_power() computes SE and power for every source type", {
  cv <- qnorm(0.95)
  cf_pilot     <- sqrt(1 / 50) + 1
  cf_pilot_int <- sqrt(1 / (2 * 75)) + 1
  se_existing  <- sqrt(268 / 500) * 1.8

  cases <- list(
    sd = list(
      source = from_sd(sd_y = 20.8), n = 500, tau = 3,
      se = 2 * 20.8 / sqrt(2 * 500)),
    sd_r2 = list(
      source = from_sd(sd_y = 20.8, r_squared = 0.05), n = 500, tau = 3,
      se = 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500)),
    sd_interaction = list(
      source = from_sd(sd_y = 2.0, interaction = TRUE), n = 223, tau = 0.67,
      se = 2 * 2.0 / sqrt(223)),
    sd_r2_interaction = list(
      source = from_sd(sd_y = 2.0, r_squared = 0.05, interaction = TRUE),
      n = 223, tau = 0.67, se = 2 * 2.0 * sqrt(1 - 0.05) / sqrt(223)),
    existing = list(
      source = from_existing(se_existing = 1.8, n_existing = 268),
      n = 500, tau = 3, se = se_existing),
    existing_interaction = list(
      source = from_existing(se_existing = 1.8, n_existing = 268,
                             interaction = TRUE),
      n = 500, tau = 3, se = se_existing),
    pilot = list(
      source = from_pilot(se_pilot = 2.0, n_pilot = 50),
      n = 500, tau = 3, se = sqrt(50 / 500) * cf_pilot * 2.0),
    pilot_interaction = list(
      source = from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE),
      n = 375, tau = 0.67, se = sqrt(75 / 375) * cf_pilot_int * 0.40)
  )

  for (nm in names(cases)) {
    k <- cases[[nm]]
    res <- find_power(k$source, n_planned = k$n, tau = k$tau)
    expect_equal(res$se, k$se, tolerance = 0.001, info = nm)
    expect_equal(res$power, 1 - pnorm(cv - k$tau / k$se),
                 tolerance = 0.001, info = nm)
  }
})

# --- Returned object ---

test_that("find_power() returns a power_result with the expected fields", {
  s <- from_sd(sd_y = 20.8)
  res <- find_power(s, n_planned = 500, tau = 3)
  se <- 2 * 20.8 / sqrt(2 * 500)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "power")
  expect_identical(res$source, s)
  expect_equal(res$ci, 0.90)
  expect_equal(res$n_planned, 500)
  expect_equal(res$tau, 3)
  expect_equal(res$se, se, tolerance = 0.001)
  expect_equal(res$ratio, 3 / se, tolerance = 0.001)
  expect_equal(res$critical_value, qnorm(0.95), tolerance = 0.001)
  expect_true(res$power > 0 && res$power < 1)
})

# --- 95% CI ---

test_that("find_power() with 95% CI uses correct critical value", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  cv <- qnorm(0.975)  # 95% CI uses 0.975 not 0.95
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3, ci = 0.95)
  expect_equal(res$ci, 0.95)
  expect_equal(res$power, expected_power, tolerance = 0.001)
  expect_equal(res$critical_value, qnorm(0.975), tolerance = 0.001)
})

# --- Known power levels ---

test_that("find_power() returns ~80% when tau = MDE_80", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  tau_80 <- mde_factor(0.80) * se
  res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = tau_80)
  expect_equal(res$power, 0.80, tolerance = 0.01)
})

test_that("find_power() returns ~50% when tau = cv * SE", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  cv <- qnorm(0.95)
  tau_50 <- cv * se
  res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = tau_50)
  expect_equal(res$power, 0.50, tolerance = 0.01)
})

test_that("find_power() is directional (one-sided), not two-sided", {
  # SE = 2 * 50 / sqrt(2 * 200) = 5, so tau / SE = 0.1
  res <- from_sd(sd_y = 50) |> find_power(n = 200, tau = 0.5)
  expect_equal(res$se, 5)
  expect_equal(res$ratio, 0.1)
  expect_equal(res$power, 1 - pnorm(qnorm(0.95) - 0.1))
  expect_equal(res$power, 0.0612, tolerance = 1e-3)
  # A two-sided test would give about 0.10 here
  expect_lt(res$power, 0.10)
})

# --- n alias ---

test_that("find_power() accepts n as alias for n_planned", {
  res_n <- from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)
  res_np <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  expect_equal(res_n$power, res_np$power)
})

test_that("find_power() errors when both n and n_planned given", {
  expect_error(
    from_sd(sd_y = 20.8) |> find_power(n = 500, n_planned = 500, tau = 3),
    "specify either"
  )
})

test_that("find_power() errors when neither n nor n_planned given", {
  expect_error(
    from_sd(sd_y = 20.8) |> find_power(tau = 3),
    "must specify sample size"
  )
})
