# Helper: create a power_result without printing
quiet <- function(expr) invisible(capture.output(expr))

# --- Input validation --------------------------------------------------------

test_that("errors on non-power_result input", {
  expect_error(write_manual("not a result"),
               "must be a power_result object")
})

test_that("errors on interaction = TRUE", {
  quiet(res <- from_sd(sd_y = 10, interaction = TRUE) |>
    find_power(n_planned = 100, tau = 2))
  expect_error(write_manual(res),
               "does not yet support interaction designs")
})

# --- Code content by source: from_sd ----------------------------------------

test_that("from_sd: SE formula uses Rule 3", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("2 * sd_y / sqrt(2 * n)", code, fixed = TRUE))
})

test_that("from_sd with R^2 > 0: SE formula includes sqrt(1 - r_squared)", {
  quiet(res <- from_sd(sd_y = 20.8, r_squared = 0.40) |>
    find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("sqrt(1 - r_squared)", code, fixed = TRUE))
  expect_true(grepl("r_squared <- 0.4", code, fixed = TRUE))
})

test_that("from_sd with R^2 = 0: omits r_squared", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_false(grepl("r_squared", code, fixed = TRUE))
})

# --- Code content by source: from_existing -----------------------------------

test_that("from_existing: SE formula uses Rule 7", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("sqrt(n_existing / n_planned) * se_existing", code, fixed = TRUE))
})

test_that("from_existing: uses n_planned variable name", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n_planned\\s+<-", code))
})

# --- Code content by source: from_pilot --------------------------------------

test_that("from_pilot: includes conservative factor", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("cf <- sqrt(1 / n_pilot) + 1", code, fixed = TRUE))
  expect_true(grepl("cf * se_pilot", code, fixed = TRUE))
})

# --- Code content by goal: find_mde -----------------------------------------

test_that("find_mde: uses 2.5 and 3.3 constants (90% CI)", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("2.5 * se", code, fixed = TRUE))
  expect_true(grepl("3.3 * se", code, fixed = TRUE))
})

test_that("find_mde: uses 2.8 and 3.6 constants (95% CI)", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("2.8 * se", code, fixed = TRUE))
  expect_true(grepl("3.6 * se", code, fixed = TRUE))
})

test_that("find_mde: prints cat statements for SE and MDE", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("Predicted SE", code, fixed = TRUE))
  expect_true(grepl("MDE (80% power)", code, fixed = TRUE))
  expect_true(grepl("MDE (95% power)", code, fixed = TRUE))
})

# --- Code content by goal: find_power ---------------------------------------

test_that("find_power: uses 1.64 critical value (90% CI)", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("1.64 - tau / se", code, fixed = TRUE))
})

test_that("find_power: uses 1.96 critical value (95% CI)", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3, ci = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("1.96 - tau / se", code, fixed = TRUE))
})

test_that("find_power: prints cat statements for SE, ratio, power", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("Predicted SE", code, fixed = TRUE))
  expect_true(grepl("tau / SE", code, fixed = TRUE))
  expect_true(grepl("Power:", code, fixed = TRUE))
})

# --- Code content by goal: find_n -------------------------------------------

test_that("find_n from_sd: direct formula without computing SE", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("ceiling(2 * (2.5 * sd_y / tau)^2)", code, fixed = TRUE))
  expect_false(grepl("se <-", code, fixed = TRUE))
})

test_that("find_n from_sd with R^2: includes sqrt(1 - r_squared) in formula", {
  quiet(res <- from_sd(sd_y = 20.8, r_squared = 0.40) |> find_n(tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("sqrt(1 - r_squared)", code, fixed = TRUE))
  expect_true(grepl("ceiling(2 *", code, fixed = TRUE))
})

test_that("find_n from_existing: uses Rule 8 formula", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n_existing * (3.3 * se_existing / tau)^2", code, fixed = TRUE))
})

test_that("find_n from_pilot: uses Rule 10 formula with cf", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n_pilot * (2.5 * cf * se_pilot / tau)^2", code, fixed = TRUE))
})

test_that("find_n: prints n/condition and total N", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("Required n/condition", code, fixed = TRUE))
  expect_true(grepl("Total N:", code, fixed = TRUE))
})

# --- Non-standard power: qnorm fallback -------------------------------------

test_that("find_n with non-standard power: uses qnorm()", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.90))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("qnorm(", code, fixed = TRUE))
  expect_true(grepl("mde_factor", code, fixed = TRUE))
})

test_that("find_n with non-standard power: prints qnorm note", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.90))
  out <- capture.output(code <- write_manual(res))
  expect_true(any(grepl("qnorm()", out, fixed = TRUE)))
  expect_true(any(grepl("90%", out)))
})

test_that("find_n with standard power (80%): no qnorm note", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80))
  out <- capture.output(code <- write_manual(res))
  expect_false(any(grepl("qnorm()", out, fixed = TRUE)))
})

test_that("find_n with standard power (95%): no qnorm note", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_false(any(grepl("qnorm()", out, fixed = TRUE)))
})

# --- Variable naming ---------------------------------------------------------

test_that("from_sd uses 'n' variable name", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n    <-", code, fixed = TRUE))
  expect_false(grepl("n_planned", code, fixed = TRUE))
})

test_that("from_existing uses 'n_planned' variable name", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n_planned\\s+<-", code))
})

test_that("from_pilot uses 'n_planned' variable name", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("n_planned\\s+<-", code))
})

# --- Output ------------------------------------------------------------------

test_that("returns character string invisibly", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_type(code, "character")
  expect_length(code, 1)
})

test_that("output contains Manual Calculation header", {
  quiet(res <- from_sd(sd_y = 10) |>
    find_power(n_planned = 200, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(any(grepl("Manual Calculation", out)))
})

# --- 95% CI support ----------------------------------------------------------

test_that("95% CI: find_n uses 3.3 for 95% power", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_n(tau = 3, power = 0.95, ci = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("3.6", code, fixed = TRUE))
})

test_that("95% CI: find_power uses 1.96", {
  quiet(res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3, ci = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("1.96", code, fixed = TRUE))
})

# --- Integration -------------------------------------------------------------

test_that("full pipeline: from_sd |> find_mde |> write_manual", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_type(code, "character")
  expect_true(grepl("Manual power calculation", code, fixed = TRUE))
})

test_that("full pipeline: from_existing |> find_n |> write_manual", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_type(code, "character")
  expect_true(grepl("n_existing", code, fixed = TRUE))
})

test_that("full pipeline: from_pilot |> find_power |> write_manual", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_type(code, "character")
  expect_true(grepl("cf <- sqrt(1 / n_pilot) + 1", code, fixed = TRUE))
})

# --- Spec examples -----------------------------------------------------------

test_that("spec Example 1a: from_sd x find_mde", {
  quiet(res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("sd_y <- 20.8", code, fixed = TRUE))
  expect_true(grepl("n    <- 500", code, fixed = TRUE))
  expect_true(grepl("2 * sd_y / sqrt(2 * n)", code, fixed = TRUE))
  expect_true(grepl("2.5 * se", code, fixed = TRUE))
  expect_true(grepl("3.3 * se", code, fixed = TRUE))
})

test_that("spec Example 2c: from_existing x find_n (95% power)", {
  quiet(res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("se_existing <- 1.8", code, fixed = TRUE))
  expect_true(grepl("n_existing  <- 268", code, fixed = TRUE))
  expect_true(grepl("tau", code, fixed = TRUE))
  expect_true(grepl("3.3 * se_existing / tau", code, fixed = TRUE))
})

test_that("spec Example 6f: from_pilot x find_power", {
  quiet(res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_power(n_planned = 500, tau = 3))
  out <- capture.output(code <- write_manual(res))
  expect_true(grepl("se_pilot  <- 2.13", code, fixed = TRUE))
  expect_true(grepl("n_pilot   <- 85", code, fixed = TRUE))
  expect_true(grepl("n_planned <- 500", code, fixed = TRUE))
  expect_true(grepl("tau       <- 3", code, fixed = TRUE))
  expect_true(grepl("cf <- sqrt(1 / n_pilot) + 1", code, fixed = TRUE))
  expect_true(grepl("1.64 - tau / se", code, fixed = TRUE))
})
