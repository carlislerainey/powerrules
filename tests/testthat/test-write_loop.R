# Helper: create a power_result without printing
quiet <- function(expr) invisible(capture.output(expr))

# --- Input validation --------------------------------------------------------

test_that("errors on non-power_result input", {
  expect_error(write_loop("not a result"),
               "must be a power_result object")
})

test_that("errors on interaction = TRUE", {
  quiet(res <- from_sd(sd_y = 10, interaction = TRUE) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_loop(res),
               "does not yet support interaction designs")
})

test_that("errors on invalid tau (negative)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_loop(res, tau = -1),
               "must be a positive number")
})

test_that("errors on invalid tau (NA)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_loop(res, tau = NA),
               "must be a positive number")
})

test_that("errors on invalid tau (non-numeric)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_loop(res, tau = "big"),
               "must be a positive number")
})

# --- Tau extraction ----------------------------------------------------------

test_that("uses tau from find_power result", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("tau  <- 3", code, fixed = TRUE))
})

test_that("uses tau from find_n result", {
  quiet(res <- from_sd(sd_y = 10) |> find_n(tau = 4))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("tau  <- 4", code, fixed = TRUE))
})

test_that("defaults to mde_80 for find_mde result", {
  quiet(res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200))
  out <- capture.output(code <- write_loop(res))
  # Code should contain the formatted mde_80 value
  mde_str <- formatC(res$mde_80, format = "f", digits = 2)
  expect_true(grepl(mde_str, code, fixed = TRUE))
  # Output should contain the note about using mde_80
  expect_true(any(grepl("MDE at 80% power", out)))
})

test_that("accepts tau override for find_mde", {
  quiet(res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200))
  out <- capture.output(code <- write_loop(res, tau = 5))
  expect_true(grepl("tau  <- 5", code, fixed = TRUE))
  # Should NOT show the mde note when tau is overridden
  expect_false(any(grepl("MDE at 80% power", out)))
})

test_that("accepts tau override for find_power", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res, tau = 7))
  expect_true(grepl("tau  <- 7", code, fixed = TRUE))
})

# --- Code content ------------------------------------------------------------

test_that("contains library(sandwich)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("library(sandwich)", code, fixed = TRUE))
})

test_that("contains vcovHC with HC2", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("vcovHC(fit, type = \"HC2\")", code, fixed = TRUE))
})

test_that("contains set.seed(1234)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("set.seed(1234)", code, fixed = TRUE))
})

test_that("contains n_sims <- 500", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("n_sims <- 500", code, fixed = TRUE))
})

test_that("contains potential outcomes structure", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("y0 <- rnorm", code, fixed = TRUE))
  expect_true(grepl("y1 <- y0 + tau", code, fixed = TRUE))
})

test_that("contains correct n value", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 221, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("n    <- 221", code, fixed = TRUE))
})

# --- SD handling -------------------------------------------------------------

test_that("from_sd: uses sd_y directly", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("sd_y <- 20.8", code, fixed = TRUE))
  expect_true(grepl("SD(Y) in control group", code, fixed = TRUE))
})

test_that("from_sd with R^2 > 0: uses effective SD", {
  quiet(res <- from_sd(sd_y = 20.8, r_squared = 0.36) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expected_sd <- 20.8 * sqrt(1 - 0.36)
  sd_str <- formatC(expected_sd, format = "f", digits = 2)
  expect_true(grepl(sd_str, code, fixed = TRUE))
  expect_true(grepl("effective SD", code, fixed = TRUE))
})

test_that("from_existing: back-calculates SD", {
  quiet(res <- from_existing(se_existing = 2, n_existing = 100) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("back-calculated", code, fixed = TRUE))
})

test_that("from_pilot: back-calculates SD", {
  quiet(res <- from_pilot(se_pilot = 2, n_pilot = 50) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("back-calculated", code, fixed = TRUE))
})

# --- Back-calculation notes --------------------------------------------------

test_that("from_sd: no back-calculation note in printed output", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_false(any(grepl("back-calculated SD", out)))
})

test_that("from_existing: prints back-calculation note", {
  quiet(res <- from_existing(se_existing = 2, n_existing = 100) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(any(grepl("back-calculated SD", out)))
})

test_that("from_pilot: prints back-calculation note", {
  quiet(res <- from_pilot(se_pilot = 2, n_pilot = 50) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(any(grepl("back-calculated SD", out)))
})

# --- One-sided test ----------------------------------------------------------

test_that("90% CI: uses size-0.05 test", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("p_val < 0.05", code, fixed = TRUE))
  expect_true(grepl("size-0.05 test", code, fixed = TRUE))
})

test_that("95% CI: uses size-0.025 test", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3, ci = 0.95))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("p_val < 0.025", code, fixed = TRUE))
  expect_true(grepl("size-0.025 test", code, fixed = TRUE))
})

# --- Expected power comments -------------------------------------------------

test_that("find_power: expected power comment matches result", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expected_pct <- round(res$power * 100)
  expect_true(grepl(paste0("Expected: ~", expected_pct, "%"), code, fixed = TRUE))
})

test_that("find_mde: expected power is ~80%", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("Expected: ~80%", code, fixed = TRUE))
})

test_that("find_n: expected power matches requested power", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("Expected: ~95%", code, fixed = TRUE))
})

# --- Output ------------------------------------------------------------------

test_that("returns character string invisibly", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_type(code, "character")
  expect_length(code, 1)
})

test_that("output contains For-Loop Simulation header", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(any(grepl("For-Loop Simulation", out)))
})

# --- Integration -------------------------------------------------------------

test_that("full pipeline: from_sd |> find_power |> write_loop", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_type(code, "character")
  expect_true(grepl("library(sandwich)", code, fixed = TRUE))
})

test_that("full pipeline: from_sd |> find_mde |> write_loop", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_loop(res))
  expect_type(code, "character")
  expect_true(grepl("library(sandwich)", code, fixed = TRUE))
})

test_that("full pipeline: from_existing |> find_n |> write_loop", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_loop(res))
  expect_type(code, "character")
  expect_true(grepl("library(sandwich)", code, fixed = TRUE))
})

test_that("full pipeline: from_pilot |> find_power |> write_loop", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_type(code, "character")
  expect_true(any(grepl("back-calculated SD", out)))
})

# --- Spec examples -----------------------------------------------------------

test_that("spec Example 6a: from_sd x find_power", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("n    <- 500", code, fixed = TRUE))
  expect_true(grepl("tau  <- 3", code, fixed = TRUE))
  expect_true(grepl("sd_y <- 20.8", code, fixed = TRUE))
  expect_true(grepl("p_val < 0.05", code, fixed = TRUE))
  expect_true(grepl("Expected: ~74%", code, fixed = TRUE))
})

test_that("spec Example 1a: from_sd x find_mde (default tau = mde_80)", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("n    <- 500", code, fixed = TRUE))
  expect_true(grepl("sd_y <- 20.8", code, fixed = TRUE))
  expect_true(grepl("MDE at 80% power", code, fixed = TRUE))
  expect_true(grepl("Expected: ~80%", code, fixed = TRUE))
})

test_that("spec Example 2c: from_existing x find_n", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_loop(res))
  expect_true(grepl("tau  <- 3", code, fixed = TRUE))
  expect_true(grepl("Expected: ~95%", code, fixed = TRUE))
  # n should be the computed value
  n_str <- fmt_val(res$n_planned)
  expect_true(grepl(paste0("n    <- ", n_str), code, fixed = TRUE))
})
