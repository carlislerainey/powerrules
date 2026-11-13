# --- DeclareDesign code generator -------------------------------------------

#' Generate DeclareDesign code for a power analysis
#'
#' Prints copy-pasteable DeclareDesign code that reproduces the power
#' calculation through simulation. The generated code follows Declaration 18.1
#' in Blair, Coppock, and Humphreys (2023).
#'
#' @param result A `"power_result"` object from `find_mde()`,
#'   `find_power()`, or `find_n()`.
#' @param tau Effect size for the DeclareDesign simulation. Defaults to
#'   `result$tau` for `find_power`/`find_n` results, or
#'   `result$mde_80` for `find_mde` results (with a note).
#' @return The generated code as a character string (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_power(n = 268, tau = 2.5) |> write_DeclareDesign()
write_DeclareDesign <- function(result, tau = NULL) {

  check_result(result, "write_DeclareDesign")
  sim <- sim_setup(result, tau)

  N_str <- fmt_val(2 * result$n_planned)
  n_per_str <- fmt_val(result$n_planned)

  # DeclareDesign's two-sided p-value threshold
  p_threshold <- if (result$ci == 0.90) "0.10" else "0.05"

  # --- Build code string (no indentation; printed with 2-space prefix) ---
  code <- paste0(
    "library(DeclareDesign)\n",
    "\n",
    "# Declare a two-arm randomized experiment and diagnose its power\n",
    "# via simulation (see Declaration 18.1 in Blair, Coppock, and\n",
    "# Humphreys, 2023, Ch. 18).\n",
    "\n",
    "my_design <-\n",
    "  # Model: ", N_str, " total respondents (", n_per_str, " per condition).\n",
    "  # Y = tau * Z + noise, where SD(Y) = ", sim$sd_str, " in the\n",
    "  # control group (Rule 3).\n",
    "  declare_model(\n",
    "    N = ", N_str, ",\n",
    "    U = rnorm(N),\n",
    "    potential_outcomes(Y ~ ", sim$tau_str, " * Z + ", sim$sd_str, " * U)\n",
    "  ) +\n",
    "  # Inquiry: the average treatment effect.\n",
    "  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +\n",
    "  # Data strategy: complete random assignment, half to treatment.\n",
    "  declare_assignment(Z = complete_ra(N, prob = 0.5)) +\n",
    "  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +\n",
    "  # Answer strategy: difference-in-means.\n",
    "  declare_estimator(Y ~ Z, inquiry = \"ATE\")\n",
    "\n",
    "# Simulate 500 experiments.\n",
    "#   IMPORTANT NOTE: DeclareDesign does not easily\n",
    "#   support one-sided tests, so power below uses two-sided\n",
    "#   tests. This closely matches the directional power from\n",
    "#   powerrules (Rule 2) above ~20% power. Below that, the\n",
    "#   two-sided power will be somewhat higher because it counts\n",
    "#   significant results in the wrong direction as well.\n",
    "diagnosis <- diagnose_design(my_design, sims = 500,\n",
    "  diagnosands = declare_diagnosands(\n",
    "    power = mean(p.value <= ", p_threshold, ")\n",
    "  ))\n",
    "diagnosis"
  )

  # --- Print formatted output ---
  cat(rule_line("DeclareDesign Code (Declaration 18.1)"), "\n")
  print_sim_notes(sim, "write_DeclareDesign", "DeclareDesign code")
  print_code(code)

  cat("\n")
  cat(rule_line("Note on power definition"), "\n")
  print_note(
    "DeclareDesign does not easily support one-sided tests, so ",
    "power above uses two-sided tests. This closely matches the ",
    "directional power from powerrules (Rule 2) ",
    "above about 20% power. Below that, the two-sided power will ",
    "be somewhat higher because it counts significant results in ",
    "the wrong direction as well."
  )

  cat("\n")
  cat(rule_line("Reference"), "\n")
  print_note(
    "See Declaration 18.1 in Blair, Coppock, and Humphreys (2023), ",
    "\"Research Design in the Social Sciences,\" Princeton University ",
    "Press, Chapter 18."
  )

  invisible(code)
}
