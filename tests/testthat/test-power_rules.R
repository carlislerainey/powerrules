# Tests for the power_rules() interactive guide. run_guide() (in
# helper-power_rules.R) mocks the ask_* functions and check_interactive(),
# allowing full orchestration testing.

# --- SD source + find_mde ----------------------------------------------------

test_that("power_rules: sd source, find_mde, treatment, no R^2", {
  # Menu: treatment(a), mde(a), sd(a), 90%CI(a), no_r2(a)
  g <- run_guide(menu = c("a", "a", "a", "a", "a"), numbers = c(20.8, 500))
  res <- g$res

  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "mde")
  expect_equal(res$source$type, "sd")
  expect_equal(res$source$sd_y, 20.8)
  expect_equal(res$source$r_squared, 0)
  expect_false(res$source$interaction)
  expect_equal(res$n_planned, 500)
  expect_equal(res$ci, 0.90)

  # Pipeline code appears in output
  expect_true(any(grepl("from_sd.*find_mde", g$out)))

  # ... and matches build_pipeline_code()
  expected_pipeline <- build_pipeline_code("sd", "mde", list(
    sd_y = 20.8, r_squared = 0, interaction = FALSE,
    ci = 0.90, n_planned = 500
  ))
  expect_true(any(grepl(expected_pipeline, g$out, fixed = TRUE)))
})

test_that("power_rules: sd source, find_mde, treatment, with R^2", {
  g <- run_guide(menu = c("a", "a", "a", "a", "b"), numbers = c(20.8, 500),
                 r_squared = 0.05)
  expect_equal(g$res$source$r_squared, 0.05)
  expect_true(any(grepl("r_squared", g$out)))
})

# --- SD source + find_n ------------------------------------------------------

test_that("power_rules: sd source, find_n, interaction, custom power", {
  # Menu: interaction(b), n(c), sd(a), 90%CI(a), custom_power(d), no_r2(a)
  g <- run_guide(menu = c("b", "c", "a", "a", "d", "a"),
                 numbers = c(2.0, 0.67), power = 0.85)
  res <- g$res

  expect_equal(res$goal, "n")
  expect_true(res$source$interaction)
  expect_equal(res$power, 0.85)
  expect_equal(res$tau, 0.67)
  expect_true(any(grepl("interaction = TRUE", g$out)))
  expect_true(any(grepl("power = 0.85", g$out)))
})

test_that("power_rules: sd source, find_n, treatment, 80% power preset", {
  # Menu: treatment(a), n(c), sd(a), 90%CI(a), 80%_power(a), no_r2(a)
  g <- run_guide(menu = c("a", "c", "a", "a", "a", "a"),
                 numbers = c(20.8, 3))

  expect_equal(g$res$goal, "n")
  expect_equal(g$res$power, 0.80)
  # Default power omitted from pipeline
  pipeline_lines <- g$out[grepl("from_sd.*find_n", g$out)]
  expect_true(length(pipeline_lines) > 0)
  expect_false(any(grepl("power", pipeline_lines)))
})

# --- Existing source ---------------------------------------------------------

test_that("power_rules: existing source, find_power, treatment", {
  # Menu: treatment(a), power(b), existing(b), 90%CI(a)
  g <- run_guide(menu = c("a", "b", "b", "a"),
                 numbers = c(1.8, 268, 500, 3))
  res <- g$res

  expect_equal(res$goal, "power")
  expect_equal(res$source$type, "existing")
  expect_equal(res$source$se_existing, 1.8)
  expect_equal(res$source$n_existing, 268)
  expect_equal(res$n_planned, 500)
  expect_equal(res$tau, 3)
  expect_true(any(grepl("from_existing.*find_power", g$out)))
})

test_that("power_rules: existing source, find_mde, interaction", {
  # Menu: interaction(b), mde(a), existing(b), 90%CI(a)
  g <- run_guide(menu = c("b", "a", "b", "a"), numbers = c(1.8, 268, 500))

  expect_equal(g$res$goal, "mde")
  expect_equal(g$res$source$type, "existing")
  expect_true(g$res$source$interaction)
  expect_true(any(grepl("interaction = TRUE", g$out)))
  expect_true(any(grepl("from_existing.*find_mde", g$out)))
  expect_true(any(grepl("2x2 factorial", g$out)))
})

# --- Pilot source ------------------------------------------------------------

test_that("power_rules: pilot source, find_n, treatment, 95% CI", {
  # Menu: treatment(a), n(c), pilot(c), 95%CI(b), 95%_power(c)
  g <- run_guide(menu = c("a", "c", "c", "b", "c"),
                 numbers = c(2.13, 85, 3))
  res <- g$res

  expect_equal(res$goal, "n")
  expect_equal(res$source$type, "pilot")
  expect_equal(res$ci, 0.95)
  expect_equal(res$power, 0.95)
  expect_equal(res$n_planned, 685)
  expect_true(any(grepl("ci = 0.95", g$out)))
})

test_that("power_rules: pilot source, find_mde, interaction", {
  # Menu: interaction(b), mde(a), pilot(c), 90%CI(a)
  g <- run_guide(menu = c("b", "a", "c", "a"), numbers = c(0.40, 75, 375))
  res <- g$res

  expect_equal(res$goal, "mde")
  expect_equal(res$source$type, "pilot")
  expect_true(res$source$interaction)
  expect_equal(res$n_planned, 375)
  expect_true(any(grepl("interaction = TRUE", g$out)))
  expect_true(any(grepl("from_pilot.*find_mde", g$out)))
})

# --- Result correctness ------------------------------------------------------

test_that("power_rules produces same result as direct pipeline call", {
  # Menu: treatment(a), mde(a), existing(b), 90%CI(a)
  guided <- run_guide(menu = c("a", "a", "b", "a"),
                      numbers = c(1.67, 502, 500))$res
  capture.output(
    direct <- from_existing(se_existing = 1.67, n_existing = 502) |>
      find_mde(n_planned = 500)
  )

  expect_equal(guided$se, direct$se)
  expect_equal(guided$mde_80, direct$mde_80)
  expect_equal(guided$mde_95, direct$mde_95)
})

# --- check_interactive guard -------------------------------------------------

test_that("power_rules errors in non-interactive session without mock", {
  # Without mocking check_interactive, it should error
  expect_error(power_rules(), "interactive")
})

# --- Re-asking for unusable answers ------------------------------------------

test_that("power_rules: re-asks for a custom power at or below the test size", {
  # Menu: treatment(a), sample size(c), sd(a), 90%CI(a), custom(d), no_r2(a)
  g <- run_guide(menu = c("a", "c", "a", "a", "d", "a"),
                 numbers = c(20.8, 3), power = c(0.03, 0.85))
  expect_equal(g$n_power_asked, 2L)
  expect_equal(g$res$power, 0.85)
  expect_true(any(grepl("greater than 0.05", g$out, fixed = TRUE)))
})

test_that("power_rules: re-asks for a whole-number sample size", {
  # Numbers: sd_y=20.8, n_planned=250.5 (rejected), n_planned=250
  g <- run_guide(menu = c("a", "a", "a", "a", "a"),
                 numbers = c(20.8, 250.5, 250))
  expect_equal(g$res$n_planned, 250)
  expect_true(any(grepl("whole number", g$out, fixed = TRUE)))
})
