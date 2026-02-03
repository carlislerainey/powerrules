# Tests for build_pipeline_code() — the extracted pipeline string builder.

# --- SD source ---------------------------------------------------------------

test_that("pipeline: sd + mde, defaults", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 n_planned = 500)
  code <- build_pipeline_code("sd", "mde", inputs)
  expect_equal(code, "from_sd(sd_y = 20.8) |> find_mde(n = 500)")
})

test_that("pipeline: sd + mde, with r_squared", {
  inputs <- list(sd_y = 20.8, r_squared = 0.05, interaction = FALSE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("sd", "mde", inputs)
  expect_equal(code,
    "from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n = 500)")
})

test_that("pipeline: sd + mde, interaction", {
  inputs <- list(sd_y = 2.0, interaction = TRUE, ci = 0.90,
                 n_planned = 375)
  code <- build_pipeline_code("sd", "mde", inputs)
  expect_equal(code,
    "from_sd(sd_y = 2, interaction = TRUE) |> find_mde(n = 375)")
})

test_that("pipeline: sd + power", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 n_planned = 500, tau = 3)
  code <- build_pipeline_code("sd", "power", inputs)
  expect_equal(code,
    "from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)")
})

test_that("pipeline: sd + n, defaults", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 tau = 3, power = 0.80)
  code <- build_pipeline_code("sd", "n", inputs)
  expect_equal(code, "from_sd(sd_y = 20.8) |> find_n(tau = 3)")
})

test_that("pipeline: sd + n, 95% power", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 tau = 3, power = 0.95)
  code <- build_pipeline_code("sd", "n", inputs)
  expect_equal(code,
    "from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)")
})

test_that("pipeline: sd + n, 95% CI", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.95,
                 tau = 3, power = 0.80)
  code <- build_pipeline_code("sd", "n", inputs)
  expect_equal(code,
    "from_sd(sd_y = 20.8) |> find_n(tau = 3, ci = 0.95)")
})

test_that("pipeline: sd + n, interaction + r_squared + non-default power + 95% CI", {
  inputs <- list(sd_y = 2.0, r_squared = 0.40, interaction = TRUE,
                 ci = 0.95, tau = 0.67, power = 0.90)
  code <- build_pipeline_code("sd", "n", inputs)
  expect_equal(code,
    "from_sd(sd_y = 2, r_squared = 0.4, interaction = TRUE) |> find_n(tau = 0.67, power = 0.9, ci = 0.95)")
})

# --- Existing source ---------------------------------------------------------

test_that("pipeline: existing + mde", {
  inputs <- list(se_existing = 1.8, n_existing = 268, interaction = FALSE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("existing", "mde", inputs)
  expect_equal(code,
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_mde(n_planned = 500)")
})

test_that("pipeline: existing + power", {
  inputs <- list(se_existing = 1.8, n_existing = 268, interaction = FALSE,
                 ci = 0.90, n_planned = 500, tau = 3)
  code <- build_pipeline_code("existing", "power", inputs)
  expect_equal(code,
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_power(n_planned = 500, tau = 3)")
})

test_that("pipeline: existing + n", {
  inputs <- list(se_existing = 1.8, n_existing = 268, interaction = FALSE,
                 ci = 0.90, tau = 3, power = 0.95)
  code <- build_pipeline_code("existing", "n", inputs)
  expect_equal(code,
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_n(tau = 3, power = 0.95)")
})

test_that("pipeline: existing + mde, interaction", {
  inputs <- list(se_existing = 1.8, n_existing = 268, interaction = TRUE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("existing", "mde", inputs)
  expect_equal(code,
    "from_existing(se_existing = 1.8, n_existing = 268, interaction = TRUE) |> find_mde(n_planned = 500)")
})

test_that("pipeline: existing + n, interaction", {
  inputs <- list(se_existing = 1.8, n_existing = 268, interaction = TRUE,
                 ci = 0.90, tau = 3, power = 0.95)
  code <- build_pipeline_code("existing", "n", inputs)
  expect_equal(code,
    "from_existing(se_existing = 1.8, n_existing = 268, interaction = TRUE) |> find_n(tau = 3, power = 0.95)")
})

# --- Pilot source ------------------------------------------------------------

test_that("pipeline: pilot + mde", {
  inputs <- list(se_pilot = 2.13, n_pilot = 85, interaction = FALSE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("pilot", "mde", inputs)
  expect_equal(code,
    "from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_mde(n_planned = 500)")
})

test_that("pipeline: pilot + power, interaction", {
  inputs <- list(se_pilot = 0.40, n_pilot = 75, interaction = TRUE,
                 ci = 0.90, n_planned = 375, tau = 0.67)
  code <- build_pipeline_code("pilot", "power", inputs)
  expect_equal(code,
    "from_pilot(se_pilot = 0.4, n_pilot = 75, interaction = TRUE) |> find_power(n_planned = 375, tau = 0.67)")
})

test_that("pipeline: pilot + n", {
  inputs <- list(se_pilot = 2.13, n_pilot = 85, interaction = FALSE,
                 ci = 0.90, tau = 3, power = 0.95)
  code <- build_pipeline_code("pilot", "n", inputs)
  expect_equal(code,
    "from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_n(tau = 3, power = 0.95)")
})

# --- Edge cases: r_squared = 0 vs NULL vs missing ---------------------------

test_that("pipeline: sd with r_squared = 0 omits it", {
  inputs <- list(sd_y = 20.8, r_squared = 0, interaction = FALSE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("sd", "mde", inputs)
  expect_false(grepl("r_squared", code))
})

test_that("pipeline: sd with r_squared = NULL omits it", {
  inputs <- list(sd_y = 20.8, r_squared = NULL, interaction = FALSE,
                 ci = 0.90, n_planned = 500)
  code <- build_pipeline_code("sd", "mde", inputs)
  expect_false(grepl("r_squared", code))
})

test_that("pipeline: default ci omitted, non-default included", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 n_planned = 500)
  code90 <- build_pipeline_code("sd", "mde", inputs)
  expect_false(grepl("ci", code90))

  inputs$ci <- 0.95
  code95 <- build_pipeline_code("sd", "mde", inputs)
  expect_true(grepl("ci = 0.95", code95))
})

test_that("pipeline: default power omitted, non-default included", {
  inputs <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                 tau = 3, power = 0.80)
  code80 <- build_pipeline_code("sd", "n", inputs)
  expect_false(grepl("power", code80))

  inputs$power <- 0.90
  code90 <- build_pipeline_code("sd", "n", inputs)
  expect_true(grepl("power = 0.9", code90))
})
