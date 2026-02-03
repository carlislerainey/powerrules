test_that("find_mde() from SD computes correct SE (no adjustment)", {
  # SE = 2 * 20.8 / sqrt(2 * 500) = 41.6 / sqrt(1000)
  expected_se <- 2 * 20.8 / sqrt(2 * 500)
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_equal(res$se, expected_se, tolerance = 0.001)
})

test_that("find_mde() from SD with adjustment computes correct SE", {
  # SE = 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500)
  expected_se <- 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500)
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n_planned = 500)
  expect_equal(res$se, expected_se, tolerance = 0.001)
})

test_that("find_mde() from SD computes correct MDE at 80% power", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  expected <- mde_factor(0.80) * se
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_equal(res$mde_80, expected, tolerance = 0.001)
})

test_that("find_mde() from SD computes correct MDE at 95% power", {
  se <- 2 * 20.8 / sqrt(2 * 500)
  expected <- mde_factor(0.95) * se
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_equal(res$mde_95, expected, tolerance = 0.001)
})

test_that("find_mde() from existing study computes correct SE", {
  # SE = sqrt(268 / 500) * 1.8
  expected_se <- sqrt(268 / 500) * 1.8
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_mde(n_planned = 500)
  expect_equal(res$se, expected_se, tolerance = 0.001)
})

test_that("find_mde() from pilot computes correct SE (conservative)", {
  # SE = sqrt(50 / 500) * (sqrt(1/50) + 1) * 2.0
  cf <- sqrt(1 / 50) + 1
  expected_se <- sqrt(50 / 500) * cf * 2.0
  res <- from_pilot(se_pilot = 2.0, n_pilot = 50) |>
    find_mde(n_planned = 500)
  expect_equal(res$se, expected_se, tolerance = 0.001)
})

test_that("find_mde() from SD (interaction) computes correct SE", {
  # Interaction: SE = 2 * SD / sqrt(n) (not sqrt(2*n))
  expected_se <- 2 * 2.0 / sqrt(223)
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_mde(n_planned = 223)
  expect_equal(res$se, expected_se, tolerance = 0.001)
})

test_that("find_mde() from pilot (interaction) computes correct SE", {
  # Interaction: cf_denom = 2 * n_pilot, so cf = sqrt(1/(2*75)) + 1
  cf <- sqrt(1 / (2 * 75)) + 1
  expected_se <- sqrt(75 / 375) * cf * 0.40
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_mde(n_planned = 375)
  expect_equal(res$se, expected_se, tolerance = 0.001)
  expect_equal(res$conservative_factor, cf, tolerance = 0.001)
})

test_that("find_mde() from existing (interaction=TRUE) SE matches interaction=FALSE", {
  res_f <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_mde(n_planned = 500)
  res_t <- from_existing(se_existing = 1.8, n_existing = 268,
                         interaction = TRUE) |>
    find_mde(n_planned = 500)
  expect_equal(res_f$se, res_t$se)
  expect_equal(res_f$mde_80, res_t$mde_80)
  expect_equal(res_f$mde_95, res_t$mde_95)
})

test_that("find_mde() with 95% CI uses correct MDE factor", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95)
  expect_equal(res$mde_factor_80, mde_factor(0.80, 0.95), tolerance = 0.001)
  expect_equal(res$mde_factor_95, mde_factor(0.95, 0.95), tolerance = 0.001)
})

test_that("find_mde() returns a power_result object invisibly", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "mde")
})

test_that("find_mde() result contains expected fields", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_true(all(c("goal", "source", "ci", "n_planned", "se",
                     "mde_80", "mde_95", "mde_factor_80",
                     "mde_factor_95") %in% names(res)))
})

test_that("find_mde() works with the pipe operator", {
  # This is a basic integration test
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n_planned = 500)
  expect_s3_class(res, "power_result")
  expect_true(res$mde_80 > 0)
  expect_true(res$mde_95 > res$mde_80)
})

# --- n alias tests ---

test_that("find_mde() accepts n as alias for n_planned", {
  res_n <- from_sd(sd_y = 20.8) |> find_mde(n = 500)
  res_np <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_equal(res_n$se, res_np$se)
  expect_equal(res_n$mde_80, res_np$mde_80)
})

test_that("find_mde() errors when both n and n_planned given", {
  expect_error(
    from_sd(sd_y = 20.8) |> find_mde(n = 500, n_planned = 500),
    "specify either"
  )
})

test_that("find_mde() errors when neither n nor n_planned given", {
  expect_error(
    from_sd(sd_y = 20.8) |> find_mde(),
    "must specify sample size"
  )
})
