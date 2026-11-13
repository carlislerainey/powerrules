# --- from_sd -----------------------------------------------------------------

test_that("from_sd find_mde: Rule 3 SE, 90% CI constants, and printed labels", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  got <- run_writer(write_manual, res)
  expect_match(got$code, "sd_y <- 20.8", fixed = TRUE)
  expect_match(got$code, "n    <- 500", fixed = TRUE)
  expect_match(got$code, "2 * sd_y / sqrt(2 * n)", fixed = TRUE)
  expect_match(got$code, "2.5 * se", fixed = TRUE)
  expect_match(got$code, "3.3 * se", fixed = TRUE)
  expect_match(got$code, "Predicted SE", fixed = TRUE)
  expect_match(got$code, "MDE (80% power)", fixed = TRUE)
  expect_match(got$code, "MDE (95% power)", fixed = TRUE)
  expect_match(got$code, "Manual power calculation", fixed = TRUE)
  expect_false(grepl("r_squared", got$code, fixed = TRUE))
})

test_that("from_sd find_mde with 95% CI uses 2.8 and 3.6", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "2.8 * se", fixed = TRUE)
  expect_match(code, "3.6 * se", fixed = TRUE)
})

test_that("from_sd with R^2 > 0 includes sqrt(1 - r_squared)", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.40) |> find_mde(n_planned = 500)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "sqrt(1 - r_squared)", fixed = TRUE)
  expect_match(code, "r_squared <- 0.4", fixed = TRUE)

  res <- from_sd(sd_y = 20.8, r_squared = 0.40) |> find_n(tau = 3)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "sqrt(1 - r_squared)", fixed = TRUE)
  expect_match(code, "ceiling(2 *", fixed = TRUE)
})

test_that("from_sd find_power: 1.64 critical value, printed labels, n variable", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  got <- run_writer(write_manual, res)
  expect_match(got$code, "1.64 - tau / se", fixed = TRUE)
  expect_match(got$code, "Predicted SE", fixed = TRUE)
  expect_match(got$code, "tau / SE", fixed = TRUE)
  expect_match(got$code, "Power:", fixed = TRUE)
  expect_match(got$code, "n    <-", fixed = TRUE)
  expect_false(grepl("n_planned", got$code, fixed = TRUE))
  expect_true(any(grepl("Manual Calculation", got$out)))
})

test_that("from_sd find_power with 95% CI uses the 1.96 critical value", {
  res <- from_sd(sd_y = 20.8) |>
    find_power(n_planned = 500, tau = 3, ci = 0.95)
  expect_match(run_writer(write_manual, res)$code, "1.96 - tau / se",
               fixed = TRUE)
})

test_that("from_sd find_n: direct formula and printed labels", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "ceiling(2 * (2.5 * sd_y / tau)^2)", fixed = TRUE)
  expect_false(grepl("se <-", code, fixed = TRUE))
  expect_match(code, "Required n/condition", fixed = TRUE)
  expect_match(code, "Total N:", fixed = TRUE)
})

test_that("from_sd find_n with 95% CI and 95% power uses 3.6", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95, ci = 0.95)
  expect_match(run_writer(write_manual, res)$code, "3.6", fixed = TRUE)
})

# --- from_existing -----------------------------------------------------------

test_that("from_existing find_mde: Pilot Rule 1 SE formula", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_mde(n_planned = 500)
  expect_match(run_writer(write_manual, res)$code,
               "sqrt(n_existing / n_planned) * se_existing", fixed = TRUE)
})

test_that("from_existing find_power uses the n_planned variable", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3)
  expect_match(run_writer(write_manual, res)$code, "n_planned\\s+<-")
})

test_that("from_existing find_n: Pilot Rule 2 formula and inputs", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "se_existing <- 1.8", fixed = TRUE)
  expect_match(code, "n_existing  <- 268", fixed = TRUE)
  expect_match(code, "n_existing * (3.3 * se_existing / tau)^2", fixed = TRUE)
  expect_match(code, "(Pilot Rule 2)", fixed = TRUE)
})

# --- from_pilot --------------------------------------------------------------

test_that("from_pilot find_mde: conservative factor and Pilot Rule 3", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_mde(n_planned = 500)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "cf <- sqrt(1 / n_pilot) + 1", fixed = TRUE)
  expect_match(code, "cf * se_pilot", fixed = TRUE)
  expect_match(code, "Predicted SE (Pilot Rule 3)", fixed = TRUE)
})

test_that("from_pilot find_power: inputs, conservative factor, 1.64", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "se_pilot  <- 2.13", fixed = TRUE)
  expect_match(code, "n_pilot   <- 85", fixed = TRUE)
  expect_match(code, "n_planned <- 500", fixed = TRUE)
  expect_match(code, "tau       <- 3", fixed = TRUE)
  expect_match(code, "cf <- sqrt(1 / n_pilot) + 1", fixed = TRUE)
  expect_match(code, "1.64 - tau / se", fixed = TRUE)
  expect_match(code, "n_planned\\s+<-")
})

test_that("from_pilot find_n: Pilot Rule 4 formula with cf", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_n(tau = 3)
  expect_match(run_writer(write_manual, res)$code,
               "n_pilot * (2.5 * cf * se_pilot / tau)^2", fixed = TRUE)
})

# --- Rule labels -------------------------------------------------------------

test_that("generated code cites the right rules, not Rules 7-10", {
  ex <- from_existing(se_existing = 1.8, n_existing = 268) |> find_n(tau = 3)
  code_ex <- run_writer(write_manual, ex)$code
  expect_match(code_ex, "(Pilot Rule 2)", fixed = TRUE)

  pi <- from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_mde(n_planned = 500)
  code_pi <- run_writer(write_manual, pi)$code
  expect_match(code_pi, "Predicted SE (Pilot Rule 3)", fixed = TRUE)

  adj <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n = 500)
  expect_match(run_writer(write_manual, adj)$code,
               "Predicted SE (Rules 3-4)", fixed = TRUE)

  expect_false(grepl("Rule (7|8|9|10)\\)", paste(code_ex, code_pi)))
})

# --- Power level in find_n ---------------------------------------------------

test_that("find_n at 80% and 95% power uses rounded constants, no qnorm note", {
  for (p in c(0.80, 0.95)) {
    res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = p)
    got <- run_writer(write_manual, res)
    expect_false(any(grepl("qnorm()", got$out, fixed = TRUE)))
  }
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_match(run_writer(write_manual, res)$code, "rounded constants",
               fixed = TRUE)
})

test_that("find_n at non-standard power falls back to qnorm()", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.90)
  got <- run_writer(write_manual, res)
  expect_match(got$code, "qnorm(", fixed = TRUE)
  expect_match(got$code, "mde_factor", fixed = TRUE)
  expect_true(any(grepl("qnorm()", got$out, fixed = TRUE)))
  expect_true(any(grepl("90%", got$out)))
  expect_false(grepl("rounded constants", got$code, fixed = TRUE))
})

test_that("non-standard power is written without rounding", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.975)
  code <- run_writer(write_manual, res)$code
  expect_match(code, "qnorm(0.95) + qnorm(0.975)", fixed = TRUE)
  expect_match(code, "97.5% power", fixed = TRUE)
})
