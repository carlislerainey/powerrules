# --- SE and MDE for every source type, interaction off and on ---

test_that("find_mde() computes SE and MDEs for every source type", {
  mf80 <- qnorm(0.95) + qnorm(0.80)
  mf95 <- qnorm(0.95) + qnorm(0.95)
  cf_pilot     <- sqrt(1 / 50) + 1
  cf_pilot_int <- sqrt(1 / (2 * 75)) + 1
  se_existing  <- sqrt(268 / 500) * 1.8

  cases <- list(
    sd = list(
      source = from_sd(sd_y = 20.8), n = 500,
      se = 2 * 20.8 / sqrt(2 * 500), cf = NULL),
    sd_r2 = list(
      source = from_sd(sd_y = 20.8, r_squared = 0.05), n = 500,
      se = 2 * 20.8 * sqrt(1 - 0.05) / sqrt(2 * 500), cf = NULL),
    sd_interaction = list(
      source = from_sd(sd_y = 2.0, interaction = TRUE), n = 223,
      se = 2 * 2.0 / sqrt(223), cf = NULL),
    sd_r2_interaction = list(
      source = from_sd(sd_y = 2.0, r_squared = 0.05, interaction = TRUE),
      n = 223, se = 2 * 2.0 * sqrt(1 - 0.05) / sqrt(223), cf = NULL),
    existing = list(
      source = from_existing(se_existing = 1.8, n_existing = 268), n = 500,
      se = se_existing, cf = NULL),
    # The SE from an existing study already encodes the design, so
    # interaction does not change it
    existing_interaction = list(
      source = from_existing(se_existing = 1.8, n_existing = 268,
                             interaction = TRUE),
      n = 500, se = se_existing, cf = NULL),
    pilot = list(
      source = from_pilot(se_pilot = 2.0, n_pilot = 50), n = 500,
      se = sqrt(50 / 500) * cf_pilot * 2.0, cf = cf_pilot),
    pilot_interaction = list(
      source = from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE),
      n = 375, se = sqrt(75 / 375) * cf_pilot_int * 0.40, cf = cf_pilot_int)
  )

  for (nm in names(cases)) {
    k <- cases[[nm]]
    res <- find_mde(k$source, n_planned = k$n)
    expect_equal(res$se, k$se, tolerance = 0.001, info = nm)
    expect_equal(res$mde_80, mf80 * k$se, tolerance = 0.001, info = nm)
    expect_equal(res$mde_95, mf95 * k$se, tolerance = 0.001, info = nm)
    expect_equal(res$conservative_factor, k$cf, tolerance = 0.001, info = nm)
  }
})

# --- Returned object ---

test_that("find_mde() returns a power_result with the expected fields", {
  s <- from_sd(sd_y = 20.8)
  res <- find_mde(s, n_planned = 500)
  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "mde")
  expect_identical(res$source, s)
  expect_equal(res$ci, 0.90)
  expect_equal(res$n_planned, 500)
  expect_equal(res$mde_factor_80, 2.4863, tolerance = 0.001)
  expect_equal(res$mde_factor_95, 3.2897, tolerance = 0.001)
  expect_gt(res$mde_95, res$mde_80)
  expect_gt(res$mde_80, 0)
})

# --- 95% CI ---

test_that("find_mde() with 95% CI uses the 95% CI MDE factors", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95)
  se <- 2 * 20.8 / sqrt(2 * 500)
  expect_equal(res$ci, 0.95)
  expect_equal(res$mde_factor_80, 2.8016, tolerance = 0.001)
  expect_equal(res$mde_factor_95, 3.6049, tolerance = 0.001)
  expect_equal(res$mde_factor_80, mde_factor(0.80, 0.95), tolerance = 0.001)
  expect_equal(res$mde_factor_95, mde_factor(0.95, 0.95), tolerance = 0.001)
  expect_equal(res$mde_80, 2.8016 * se, tolerance = 0.001)
  expect_equal(res$mde_95, 3.6049 * se, tolerance = 0.001)
})

# --- n alias ---

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
