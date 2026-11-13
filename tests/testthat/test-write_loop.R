# --- Code structure ----------------------------------------------------------

test_that("90% CI code: sandwich, HC2, seed, potential outcomes, size-0.05 test", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 221, tau = 3)
  got <- run_writer(write_loop, res)
  expect_match(got$code, "library(sandwich)", fixed = TRUE)
  expect_match(got$code, "vcovHC(fit, type = \"HC2\")", fixed = TRUE)
  expect_match(got$code, "set.seed(1234)", fixed = TRUE)
  expect_match(got$code, "n_sims <- 500", fixed = TRUE)
  expect_match(got$code, "y0 <- rnorm", fixed = TRUE)
  expect_match(got$code, "y1 <- y0 + tau", fixed = TRUE)
  expect_match(got$code, "n    <- 221", fixed = TRUE)
  expect_match(got$code, "p_val < 0.05", fixed = TRUE)
  expect_match(got$code, "size-0.05 test", fixed = TRUE)
  expect_true(any(grepl("For-Loop Simulation", got$out)))
})

test_that("95% CI code uses a size-0.025 test", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3, ci = 0.95)
  code <- run_writer(write_loop, res)$code
  expect_match(code, "p_val < 0.025", fixed = TRUE)
  expect_match(code, "size-0.025 test", fixed = TRUE)
})

# --- SD handling -------------------------------------------------------------

test_that("from_sd find_power: sd_y used directly, expected power matches result", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_loop, res)$code
  expect_match(code, "n    <- 500", fixed = TRUE)
  expect_match(code, "tau  <- 3", fixed = TRUE)
  expect_match(code, "sd_y <- 20.8", fixed = TRUE)
  expect_match(code, "SD(Y) in control group", fixed = TRUE)
  expect_match(code, "p_val < 0.05", fixed = TRUE)
  expect_match(code, "Expected: ~74%", fixed = TRUE)
  expect_match(code, paste0("Expected: ~", round(res$power * 100), "%"),
               fixed = TRUE)
})

test_that("from_sd with R^2 > 0 uses the effective SD", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.36) |>
    find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_loop, res)$code
  expect_match(code, formatC(20.8 * sqrt(1 - 0.36), format = "f", digits = 2),
               fixed = TRUE)
  expect_match(code, "effective SD", fixed = TRUE)
})

test_that("from_existing back-calculates the SD from the predicted SE", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_loop, res)$code
  expect_match(code, "back-calculated", fixed = TRUE)
  expect_match(code, "sd_y <- 20.84", fixed = TRUE)
})

test_that("from_pilot back-calculates the SD", {
  res <- from_pilot(se_pilot = 2, n_pilot = 50) |>
    find_power(n_planned = 200, tau = 3)
  expect_match(run_writer(write_loop, res)$code, "back-calculated", fixed = TRUE)
})

# --- Expected power comments -------------------------------------------------

test_that("find_mde: tau is the 80% MDE and expected power is ~80%", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  code <- run_writer(write_loop, res)$code
  expect_match(code, "n    <- 500", fixed = TRUE)
  expect_match(code, "sd_y <- 20.8", fixed = TRUE)
  expect_match(code, "MDE at 80% power", fixed = TRUE)
  expect_match(code, "Expected: ~80%", fixed = TRUE)
})

test_that("find_n: expected power matches requested power and n is computed", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  expect_match(run_writer(write_loop, res)$code, "Expected: ~95%", fixed = TRUE)

  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, power = 0.95)
  code <- run_writer(write_loop, res)$code
  expect_match(code, "tau  <- 3", fixed = TRUE)
  expect_match(code, "Expected: ~95%", fixed = TRUE)
  expect_match(code, paste0("n    <- ", fmt_val(res$n_planned)), fixed = TRUE)
  expect_match(code, "library(sandwich)", fixed = TRUE)
})

test_that("expected power is recomputed when tau is overridden", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  code <- run_writer(write_loop, res, tau = 4)$code
  expected <- round(100 * (1 - pnorm(qnorm(0.95) - 4 / res$se)))
  expect_match(code, paste0("Expected: ~", expected, "%"), fixed = TRUE)
  expect_false(grepl("Expected: ~74%", code, fixed = TRUE))

  mde <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  code <- run_writer(write_loop, mde, tau = mde$mde_95)$code
  expect_match(code, "Expected: ~95%", fixed = TRUE)
})
