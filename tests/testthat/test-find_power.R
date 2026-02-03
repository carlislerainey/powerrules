test_that("find_power() from SD computes correct power", {
  # SD=20.8, n=500, tau=3, no adjustment
  se <- 2 * 20.8 / sqrt(2 * 500)
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() from SD with adjustment computes correct power", {
  # SD=20.8, n=500, tau=3, R^2=0.05
  se <- 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500)
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |>
    find_power(n_planned = 500, tau = 3)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() from existing study computes correct power", {
  # se=1.8, n_existing=268, n_planned=500, tau=3
  se <- sqrt(268 / 500) * 1.8
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() from pilot computes correct power (conservative)", {
  # se=2.0, n_pilot=50, n_planned=500, tau=3
  cf <- sqrt(1 / 50) + 1
  se <- sqrt(50 / 500) * cf * 2.0
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_pilot(se_pilot = 2.0, n_pilot = 50) |>
    find_power(n_planned = 500, tau = 3)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() from SD (interaction) computes correct power", {
  # SD=2.0, n=223, tau=0.67
  se <- 2 * 2.0 / sqrt(223)  # interaction: sqrt(n) not sqrt(2n)
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 0.67 / se)
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_power(n_planned = 223, tau = 0.67)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() from pilot (interaction) computes correct power", {
  # se_pilot=0.40, n_pilot=75, n_planned=375, tau=0.67
  # Interaction: cf_denom = 2*75, so cf = sqrt(1/150) + 1
  cf <- sqrt(1 / (2 * 75)) + 1
  se <- sqrt(75 / 375) * cf * 0.40
  cv <- qnorm(0.95)
  expected_power <- 1 - pnorm(cv - 0.67 / se)
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_power(n_planned = 375, tau = 0.67)
  expect_equal(res$power, expected_power, tolerance = 0.001)
})

test_that("find_power() with 95% CI uses correct critical value", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  cv <- qnorm(0.975)  # 95% CI uses 0.975 not 0.95
  expected_power <- 1 - pnorm(cv - 3 / se)
  res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3, ci = 0.95)
  expect_equal(res$power, expected_power, tolerance = 0.001)
  expect_equal(res$critical_value, qnorm(0.975), tolerance = 0.001)
})

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

test_that("find_power() returns a power_result object invisibly", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "power")
})

test_that("find_power() works with the pipe operator", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3)
  expect_s3_class(res, "power_result")
  expect_true(res$power > 0 && res$power < 1)
})

# --- n alias tests ---

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
