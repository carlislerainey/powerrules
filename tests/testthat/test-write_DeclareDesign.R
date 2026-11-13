# --- Code structure ----------------------------------------------------------

test_that("from_sd code: library, diagnose_design, N, tau, SD, no lm_lin", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 221, tau = 2.5)
  code <- run_writer(write_DeclareDesign, res)$code
  expect_match(code, "library(DeclareDesign)", fixed = TRUE)
  expect_match(code, "diagnose_design", fixed = TRUE)
  expect_match(code, "N = 442", fixed = TRUE)
  expect_match(code, "2.5 * Z", fixed = TRUE)
  expect_match(code, "10 * U", fixed = TRUE)
  expect_false(grepl("lm_lin", code, fixed = TRUE))
})

test_that("from_sd with R^2 > 0 uses the effective SD, no lm_lin", {
  res <- from_sd(sd_y = 10, r_squared = 0.36) |>
    find_power(n_planned = 200, tau = 3)
  code <- run_writer(write_DeclareDesign, res)$code
  expect_match(code, formatC(10 * sqrt(1 - 0.36), format = "f", digits = 2),
               fixed = TRUE)
  expect_false(grepl("lm_lin", code, fixed = TRUE))
})

test_that("from_existing puts the back-calculated SD in the code", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_DeclareDesign, res)$code
  expect_match(code, "library(DeclareDesign)", fixed = TRUE)
  expect_match(code, "20.84 * U", fixed = TRUE)
})

test_that("p-value cutoff follows the CI level", {
  res90 <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  res95 <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3, ci = 0.95)
  expect_match(run_writer(write_DeclareDesign, res90)$code,
               "p.value <= 0.10", fixed = TRUE)
  expect_match(run_writer(write_DeclareDesign, res95)$code,
               "p.value <= 0.05", fixed = TRUE)
})

# --- Power definition note and citation --------------------------------------

test_that("output and code explain the two-sided approximation", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  got <- run_writer(write_DeclareDesign, res)
  expect_true(any(grepl("directional power", got$out)))
  expect_true(any(grepl("two-sided tests", got$out)))
  expect_true(any(grepl("20%", got$out)))
  expect_match(got$code, "two-sided", fixed = TRUE)
  expect_match(got$code, "directional power", fixed = TRUE)
  expect_match(got$code, "~20% power", fixed = TRUE)
  expect_match(got$code, "wrong direction", fixed = TRUE)
  expect_match(got$code, "one-sided tests", fixed = TRUE)
})

test_that("output cites Blair, Coppock, and Humphreys (2023), Princeton", {
  res <- from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)
  out <- run_writer(write_DeclareDesign, res)$out
  expect_true(any(grepl("Blair, Coppock, and Humphreys", out)))
  text <- paste(out, collapse = " ")
  expect_match(text, "2023", fixed = TRUE)
  expect_match(text, "Princeton", fixed = TRUE)
  expect_false(grepl("Cambridge", text, fixed = TRUE))
})
