# Tests for build_pipeline_code() — the extracted pipeline string builder.

# Each case: label, source type, goal type, inputs, expected pipeline string.
pipeline_case <- function(label, source_type, goal_type, inputs, expected) {
  list(label = label, source_type = source_type, goal_type = goal_type,
       inputs = inputs, expected = expected)
}

pipeline_cases <- list(
  # --- SD source ---
  pipeline_case("sd + mde, defaults", "sd", "mde",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.90, n_planned = 500),
    "from_sd(sd_y = 20.8) |> find_mde(n = 500)"),
  pipeline_case("sd + mde, with r_squared", "sd", "mde",
    list(sd_y = 20.8, r_squared = 0.05, interaction = FALSE, ci = 0.90,
         n_planned = 500),
    "from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n = 500)"),
  pipeline_case("sd + mde, interaction", "sd", "mde",
    list(sd_y = 2.0, interaction = TRUE, ci = 0.90, n_planned = 375),
    "from_sd(sd_y = 2, interaction = TRUE) |> find_mde(n = 375)"),
  pipeline_case("sd + power", "sd", "power",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.90, n_planned = 500,
         tau = 3),
    "from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)"),
  pipeline_case("sd + n, defaults", "sd", "n",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.90, tau = 3, power = 0.80),
    "from_sd(sd_y = 20.8) |> find_n(tau = 3)"),
  pipeline_case("sd + n, 95% power", "sd", "n",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.90, tau = 3, power = 0.95),
    "from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)"),
  pipeline_case("sd + n, 95% CI", "sd", "n",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.95, tau = 3, power = 0.80),
    "from_sd(sd_y = 20.8) |> find_n(tau = 3, ci = 0.95)"),
  pipeline_case("sd + n, interaction + r_squared + non-default power + 95% CI",
    "sd", "n",
    list(sd_y = 2.0, r_squared = 0.40, interaction = TRUE, ci = 0.95,
         tau = 0.67, power = 0.90),
    "from_sd(sd_y = 2, r_squared = 0.4, interaction = TRUE) |> find_n(tau = 0.67, power = 0.9, ci = 0.95)"),

  # --- Existing source ---
  pipeline_case("existing + mde", "existing", "mde",
    list(se_existing = 1.8, n_existing = 268, interaction = FALSE, ci = 0.90,
         n_planned = 500),
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_mde(n_planned = 500)"),
  pipeline_case("existing + power", "existing", "power",
    list(se_existing = 1.8, n_existing = 268, interaction = FALSE, ci = 0.90,
         n_planned = 500, tau = 3),
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_power(n_planned = 500, tau = 3)"),
  pipeline_case("existing + n", "existing", "n",
    list(se_existing = 1.8, n_existing = 268, interaction = FALSE, ci = 0.90,
         tau = 3, power = 0.95),
    "from_existing(se_existing = 1.8, n_existing = 268) |> find_n(tau = 3, power = 0.95)"),
  pipeline_case("existing + mde, interaction", "existing", "mde",
    list(se_existing = 1.8, n_existing = 268, interaction = TRUE, ci = 0.90,
         n_planned = 500),
    "from_existing(se_existing = 1.8, n_existing = 268, interaction = TRUE) |> find_mde(n_planned = 500)"),
  pipeline_case("existing + n, interaction", "existing", "n",
    list(se_existing = 1.8, n_existing = 268, interaction = TRUE, ci = 0.90,
         tau = 3, power = 0.95),
    "from_existing(se_existing = 1.8, n_existing = 268, interaction = TRUE) |> find_n(tau = 3, power = 0.95)"),

  # --- Pilot source ---
  pipeline_case("pilot + mde", "pilot", "mde",
    list(se_pilot = 2.13, n_pilot = 85, interaction = FALSE, ci = 0.90,
         n_planned = 500),
    "from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_mde(n_planned = 500)"),
  pipeline_case("pilot + power, interaction", "pilot", "power",
    list(se_pilot = 0.40, n_pilot = 75, interaction = TRUE, ci = 0.90,
         n_planned = 375, tau = 0.67),
    "from_pilot(se_pilot = 0.4, n_pilot = 75, interaction = TRUE) |> find_power(n_planned = 375, tau = 0.67)"),
  pipeline_case("pilot + n", "pilot", "n",
    list(se_pilot = 2.13, n_pilot = 85, interaction = FALSE, ci = 0.90,
         tau = 3, power = 0.95),
    "from_pilot(se_pilot = 2.13, n_pilot = 85) |> find_n(tau = 3, power = 0.95)"),

  # --- Edge cases: r_squared = 0 and NULL are omitted (exact string) ---
  pipeline_case("sd with r_squared = 0 omits it", "sd", "mde",
    list(sd_y = 20.8, r_squared = 0, interaction = FALSE, ci = 0.90,
         n_planned = 500),
    "from_sd(sd_y = 20.8) |> find_mde(n = 500)"),
  pipeline_case("sd with r_squared = NULL omits it", "sd", "mde",
    list(sd_y = 20.8, r_squared = NULL, interaction = FALSE, ci = 0.90,
         n_planned = 500),
    "from_sd(sd_y = 20.8) |> find_mde(n = 500)"),
  # --- Edge cases: non-default ci and power are included ---
  pipeline_case("non-default ci is included", "sd", "mde",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.95, n_planned = 500),
    "from_sd(sd_y = 20.8) |> find_mde(n = 500, ci = 0.95)"),
  pipeline_case("non-default power 0.90 is included", "sd", "n",
    list(sd_y = 20.8, interaction = FALSE, ci = 0.90, tau = 3, power = 0.90),
    "from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.9)")
)

test_that("build_pipeline_code() builds the expected pipeline string", {
  for (case in pipeline_cases) {
    code <- build_pipeline_code(case$source_type, case$goal_type, case$inputs)
    expect_identical(code, case$expected, info = case$label)
  }
})

# --- Edge cases: omitted defaults (substring checks) --------------------------

test_that("pipeline: omitted r_squared, default ci and default power leave no trace", {
  inputs0 <- list(sd_y = 20.8, r_squared = 0, interaction = FALSE,
                  ci = 0.90, n_planned = 500)
  expect_false(grepl("r_squared", build_pipeline_code("sd", "mde", inputs0)))

  inputs_null <- list(sd_y = 20.8, r_squared = NULL, interaction = FALSE,
                      ci = 0.90, n_planned = 500)
  expect_false(grepl("r_squared",
                     build_pipeline_code("sd", "mde", inputs_null)))

  expect_false(grepl("ci", build_pipeline_code("sd", "mde", inputs_null)))

  inputs_n <- list(sd_y = 20.8, interaction = FALSE, ci = 0.90,
                   tau = 3, power = 0.80)
  expect_false(grepl("power", build_pipeline_code("sd", "n", inputs_n)))
})
