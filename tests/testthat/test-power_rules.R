# Tests for the power_rules() interactive guide.
# Uses local_mocked_bindings to mock the ask_* functions and
# check_interactive(), allowing full orchestration testing.

# Helper: create a mock for ask_menu that returns responses in sequence
make_menu_mock <- function(responses) {
  idx <- 0L
  function(prompt, choices) {
    idx <<- idx + 1L
    responses[[idx]]
  }
}

# Helper: create a mock for ask_number that returns responses in sequence
make_number_mock <- function(responses) {
  idx <- 0L
  function(prompt, hint = NULL) {
    idx <<- idx + 1L
    responses[[idx]]
  }
}

# --- SD source + find_mde, treatment effect -----------------------------------

test_that("power_rules: sd source, find_mde, treatment, no R^2", {
  # Menu responses: treatment(a), mde(a), sd(a), 90%CI(a), no_r2(a)
  menu_mock <- make_menu_mock(list("a", "a", "a", "a", "a"))
  # Number responses: sd_y=20.8, n_planned=500
  number_mock <- make_number_mock(list(20.8, 500))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_s3_class(res, "power_result")
  expect_equal(res$goal, "mde")
  expect_equal(res$source$type, "sd")
  expect_equal(res$source$sd_y, 20.8)
  expect_equal(res$source$r_squared, 0)
  expect_false(res$source$interaction)
  expect_equal(res$n_planned, 500)
  expect_equal(res$ci, 0.90)

  # Check pipeline code appears in output
  expect_true(any(grepl("from_sd.*find_mde", out)))
})

# --- SD source + find_mde, treatment effect, with R^2 ------------------------

test_that("power_rules: sd source, find_mde, treatment, with R^2", {
  menu_mock <- make_menu_mock(list("a", "a", "a", "a", "b"))
  number_mock <- make_number_mock(list(20.8, 500))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0.05,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$source$r_squared, 0.05)
  expect_true(any(grepl("r_squared", out)))
})

# --- SD source + find_n, interaction, custom power ----------------------------

test_that("power_rules: sd source, find_n, interaction, custom power", {
  # Menu: interaction(b), n(c), sd(a), 90%CI(a), custom_power(d), no_r2(a)
  menu_mock <- make_menu_mock(list("b", "c", "a", "a", "d", "a"))
  # Numbers: sd_y=2.0, tau=0.67
  number_mock <- make_number_mock(list(2.0, 0.67))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.85,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "n")
  expect_true(res$source$interaction)
  expect_equal(res$power, 0.85)
  expect_equal(res$tau, 0.67)
  expect_true(any(grepl("interaction = TRUE", out)))
  expect_true(any(grepl("power = 0.85", out)))
})

# --- Existing source + find_power, treatment ----------------------------------

test_that("power_rules: existing source, find_power, treatment", {
  # Menu: treatment(a), power(b), existing(b), 90%CI(a)
  menu_mock <- make_menu_mock(list("a", "b", "b", "a"))
  # Numbers: se_existing=1.8, n_existing=268, n_planned=500, tau=3
  number_mock <- make_number_mock(list(1.8, 268, 500, 3))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "power")
  expect_equal(res$source$type, "existing")
  expect_equal(res$source$se_existing, 1.8)
  expect_equal(res$source$n_existing, 268)
  expect_equal(res$n_planned, 500)
  expect_equal(res$tau, 3)
  expect_true(any(grepl("from_existing.*find_power", out)))
})

# --- Existing source + find_mde, interaction ----------------------------------

test_that("power_rules: existing source, find_mde, interaction", {
  # Menu: interaction(b), mde(a), existing(b), 90%CI(a)
  menu_mock <- make_menu_mock(list("b", "a", "b", "a"))
  # Numbers: se_existing=1.8, n_existing=268, n_planned=500
  number_mock <- make_number_mock(list(1.8, 268, 500))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "mde")
  expect_equal(res$source$type, "existing")
  expect_true(res$source$interaction)
  expect_true(any(grepl("interaction = TRUE", out)))
  expect_true(any(grepl("from_existing.*find_mde", out)))
  expect_true(any(grepl("2x2 factorial", out)))
})

# --- Pilot source + find_n, treatment, 95% CI --------------------------------

test_that("power_rules: pilot source, find_n, treatment, 95% CI", {
  # Menu: treatment(a), n(c), pilot(c), 95%CI(b), 95%_power(c)
  menu_mock <- make_menu_mock(list("a", "c", "c", "b", "c"))
  # Numbers: se_pilot=2.13, n_pilot=85, tau=3
  number_mock <- make_number_mock(list(2.13, 85, 3))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "n")
  expect_equal(res$source$type, "pilot")
  expect_equal(res$ci, 0.95)
  expect_equal(res$power, 0.95)
  expect_equal(res$n_planned, 685)
  expect_true(any(grepl("ci = 0.95", out)))
})

# --- Pilot source + find_mde, interaction ------------------------------------

test_that("power_rules: pilot source, find_mde, interaction", {
  # Menu: interaction(b), mde(a), pilot(c), 90%CI(a)
  menu_mock <- make_menu_mock(list("b", "a", "c", "a"))
  # Numbers: se_pilot=0.40, n_pilot=75, n_planned=375
  number_mock <- make_number_mock(list(0.40, 75, 375))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "mde")
  expect_equal(res$source$type, "pilot")
  expect_true(res$source$interaction)
  expect_equal(res$n_planned, 375)
  expect_true(any(grepl("interaction = TRUE", out)))
  expect_true(any(grepl("from_pilot.*find_mde", out)))
})

# --- SD source + find_n, treatment, 80% power (preset) ----------------------

test_that("power_rules: sd source, find_n, treatment, 80% power preset", {
  # Menu: treatment(a), n(c), sd(a), 90%CI(a), 80%_power(a), no_r2(a)
  menu_mock <- make_menu_mock(list("a", "c", "a", "a", "a", "a"))
  # Numbers: sd_y=20.8, tau=3
  number_mock <- make_number_mock(list(20.8, 3))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expect_equal(res$goal, "n")
  expect_equal(res$power, 0.80)
  # Default power omitted from pipeline
  pipeline_lines <- out[grepl("from_sd.*find_n", out)]
  expect_true(length(pipeline_lines) > 0)
  expect_false(any(grepl("power", pipeline_lines)))
})

# --- Result correctness: values match direct pipeline call -------------------

test_that("power_rules produces same result as direct pipeline call", {
  # Menu: treatment(a), mde(a), existing(b), 90%CI(a)
  menu_mock <- make_menu_mock(list("a", "a", "b", "a"))
  # Numbers: se_existing=1.67, n_existing=502, n_planned=500
  number_mock <- make_number_mock(list(1.67, 502, 500))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  capture.output(guided <- power_rules())
  capture.output(
    direct <- from_existing(se_existing = 1.67, n_existing = 502) |>
      find_mde(n_planned = 500)
  )

  expect_equal(guided$se, direct$se)
  expect_equal(guided$mde_80, direct$mde_80)
  expect_equal(guided$mde_95, direct$mde_95)
})

# --- check_interactive guard --------------------------------------------------

test_that("power_rules errors in non-interactive session without mock", {
  # Without mocking check_interactive, it should error
  expect_error(power_rules(), "interactive")
})

# --- Pipeline code in output matches build_pipeline_code ---------------------

test_that("power_rules output contains correct pipeline code", {
  menu_mock <- make_menu_mock(list("a", "a", "a", "a", "a"))
  number_mock <- make_number_mock(list(20.8, 500))

  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = menu_mock,
    ask_number = number_mock,
    ask_r_squared = function(prompt, hint = NULL) 0,
    ask_power = function(prompt, hint = NULL) 0.80,
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )

  out <- capture.output(res <- power_rules())

  expected_pipeline <- build_pipeline_code("sd", "mde", list(
    sd_y = 20.8, r_squared = 0, interaction = FALSE,
    ci = 0.90, n_planned = 500
  ))

  expect_true(any(grepl(expected_pipeline, out, fixed = TRUE)))
})
