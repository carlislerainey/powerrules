# Helper: create a power_result without printing
quiet <- function(expr) invisible(capture.output(expr))

# --- Input validation --------------------------------------------------------

test_that("errors on non-power_result input", {
  expect_error(write_DeclareDesign("not a result"),
               "must be a power_result object")
})

test_that("errors on interaction = TRUE", {
  quiet(res <- from_sd(sd_y = 10, interaction = TRUE) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_DeclareDesign(res),
               "does not yet support interaction designs")
})

test_that("errors on invalid tau (negative)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_DeclareDesign(res, tau = -1),
               "must be a positive number")
})

test_that("errors on invalid tau (NA)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_DeclareDesign(res, tau = NA),
               "must be a positive number")
})

test_that("errors on invalid tau (non-numeric)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_DeclareDesign(res, tau = "big"),
               "must be a positive number")
})

# --- Tau extraction ---------------------------------------------------------

test_that("uses tau from find_power result", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("3 \\* Z", code))
})

test_that("uses tau from find_n result", {
  quiet(res <- from_sd(sd_y = 10) |> find_n(tau = 4))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("4 \\* Z", code))
})

test_that("defaults to mde_80 for find_mde result", {
  quiet(res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200))
  out <- capture.output(code <- write_DeclareDesign(res))
  # Code should contain the formatted mde_80 value
  mde_str <- formatC(res$mde_80, format = "f", digits = 2)
  expect_true(grepl(mde_str, code, fixed = TRUE))
  # Output should contain the note about using mde_80
  expect_true(any(grepl("MDE at 80% power", out)))
})

test_that("accepts tau override for find_mde", {
  quiet(res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200))
  out <- capture.output(code <- write_DeclareDesign(res, tau = 5))
  expect_true(grepl("5 \\* Z", code))
  # Should NOT show the mde note when tau is overridden
  expect_false(any(grepl("MDE at 80% power", out)))
})

test_that("accepts tau override for find_power", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res, tau = 7))
  expect_true(grepl("7 \\* Z", code))
})

# --- Code content -----------------------------------------------------------

test_that("contains library(DeclareDesign)", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("library(DeclareDesign)", code, fixed = TRUE))
})

test_that("contains diagnose_design", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("diagnose_design", code, fixed = TRUE))
})

test_that("contains correct N (2 * n_planned) in declare_model", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 221, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("N = 442", code, fixed = TRUE))
})

test_that("contains correct tau value in potential_outcomes", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 2.5))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("2.5 \\* Z", code))
})

test_that("does NOT contain lm_lin", {
  quiet(res <- from_sd(sd_y = 10, r_squared = 0.3) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_false(grepl("lm_lin", code, fixed = TRUE))
})

test_that("from_sd with R^2 > 0: sd_coef includes sqrt(1 - r_squared)", {
  quiet(res <- from_sd(sd_y = 10, r_squared = 0.36) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expected_sd <- 10 * sqrt(1 - 0.36)  # 8.0
  sd_str <- formatC(expected_sd, format = "f", digits = 2)
  expect_true(grepl(sd_str, code, fixed = TRUE))
})

test_that("from_sd with R^2 = 0: sd_coef is just sd_y", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("10 \\* U", code))
})

# --- sd_y and warnings ------------------------------------------------------

test_that("from_sd: no back-calculation note", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_false(any(grepl("back-calculated", out)))
})

test_that("from_existing: prints back-calculation note", {
  quiet(res <- from_existing(se_existing = 2, n_existing = 100) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(any(grepl("back-calculated", out)))
})

test_that("from_pilot: prints back-calculation note", {
  quiet(res <- from_pilot(se_pilot = 2, n_pilot = 50) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(any(grepl("back-calculated", out)))
})

# --- Power definition note --------------------------------------------------

test_that("output contains power definition note", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(any(grepl("directional power", out)))
  expect_true(any(grepl("two-sided tests", out)))
  expect_true(any(grepl("20%", out)))
})

test_that("generated code comment notes two-sided approximation", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(grepl("two-sided", code))
  expect_true(grepl("directional power", code))
  expect_true(grepl("~20% power", code))
  expect_true(grepl("wrong direction", code))
  expect_true(grepl("one-sided tests", code))
})

# --- Output -----------------------------------------------------------------

test_that("returns character string invisibly", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_type(code, "character")
  expect_length(code, 1)
})

test_that("output contains Blair, Coppock, and Humphreys citation", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_true(any(grepl("Blair, Coppock, and Humphreys", out)))
})

# --- Integration ------------------------------------------------------------

test_that("full pipeline: from_sd |> find_n |> write_DeclareDesign", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 2.5))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_type(code, "character")
  expect_true(grepl("library(DeclareDesign)", code, fixed = TRUE))
})

test_that("full pipeline: from_existing |> find_power |> write_DeclareDesign", {
  quiet(res <- from_existing(se_existing = 2, n_existing = 100) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_DeclareDesign(res))
  expect_type(code, "character")
  expect_true(grepl("library(DeclareDesign)", code, fixed = TRUE))
})

test_that("write_DeclareDesign cites Blair et al. 2023 and Princeton", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3))
  out <- paste(capture.output(write_DeclareDesign(res)), collapse = " ")
  expect_true(grepl("2023", out))
  expect_true(grepl("Princeton", out))
  expect_false(grepl("Cambridge", out))
})
