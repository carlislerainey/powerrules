# End-to-end tests reproducing worked examples from Rainey (2024).
# Expected values are hardcoded from the paper and exact computation,
# NOT re-derived from the same formulas used in the source code.

test_that("Ahler & Sood: MDE from SD, no adjustment", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  expect_equal(res$se, 1.3155, tolerance = 0.01)
  expect_equal(res$mde_80, 3.2710, tolerance = 0.01)
  expect_equal(res$mde_95, 4.3276, tolerance = 0.01)
})

test_that("Ahler & Sood: MDE from SD, with adjustment (R^2=0.05)", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n_planned = 500)
  expect_equal(res$se, 1.2822, tolerance = 0.01)
})

test_that("Ahler & Sood: MDE from SD, Clifford pre-post (R^2=0.40)", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.40) |> find_mde(n_planned = 500)
  expect_equal(res$se, 1.0190, tolerance = 0.01)
  expect_equal(res$mde_80, 2.5337, tolerance = 0.01)
})

test_that("Ahler & Sood: sample size from SD (95% power)", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1041)
})

test_that("Ahler & Sood: sample size from SD with adjustment (95% power)", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.05) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 989)
})

test_that("Ahler & Sood: sample size from existing study (95% power)", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1045)
})

test_that("Broockman et al: sample size from existing study (95% power)", {
  res <- from_existing(se_existing = 1.67, n_existing = 502) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 1684)
})

test_that("Cutler et al: sample size from pilot (95% power)", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  expect_equal(res$n_planned, 570)
})

test_that("Robbins et al: sample size from SD, interaction (80% power)", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, 221)
})

test_that("Robbins et al: sample size from SD, interaction (95% power)", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.95)
  expect_equal(res$n_planned, 386)
})

test_that("Robbins et al: sample size from pilot, interaction (80% power)", {
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_equal(res$n_planned, 194)
})

test_that("Robbins et al: sample size from pilot, interaction (95% power)", {
  res <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.95)
  expect_equal(res$n_planned, 339)
})
