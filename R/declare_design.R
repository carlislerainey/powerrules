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

  # --- Validation ---
  if (!inherits(result, "power_result"))
    stop("`result` must be a power_result object ",
         "(use find_mde(), find_power(), or find_n())")

  if (result$source$interaction)
    stop("write_DeclareDesign() does not yet support interaction designs. ",
         "This feature is planned for a future version.")

  if (!is.null(tau)) {
    if (!is.numeric(tau) || length(tau) != 1 || is.na(tau) || tau <= 0)
      stop("`tau` must be a positive number")
  }

  # --- Tau extraction ---
  tau_from_mde <- FALSE
  if (is.null(tau)) {
    if (result$goal == "mde") {
      tau <- result$mde_80
      tau_from_mde <- TRUE
    } else {
      tau <- result$tau
    }
  }

  # --- sd_coef determination ---
  source <- result$source
  n_planned <- result$n_planned
  sd_backcalc <- FALSE

  if (source$type == "sd") {
    if (source$r_squared > 0) {
      sd_coef <- source$sd_y * sqrt(1 - source$r_squared)
    } else {
      sd_coef <- source$sd_y
    }
  } else {
    sd_coef <- result$se * sqrt(n_planned / 2)
    sd_backcalc <- TRUE
  }

  # --- Format values for generated code ---
  N_str <- fmt_val(2 * n_planned)
  n_per_str <- fmt_val(n_planned)

  tau_str <- if (tau_from_mde) fmt(tau) else fmt_val(tau)

  sd_str <- if (source$type == "sd" && source$r_squared == 0) {
    fmt_val(sd_coef)
  } else {
    fmt(sd_coef)
  }

  # CI / sidedness values
  ci <- result$ci
  ci_pct <- round(ci * 100)
  p_threshold <- if (ci == 0.90) "0.10" else "0.05"
  test_size <- if (ci == 0.90) "0.05" else "0.025"

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
    "  # Y = tau * Z + noise, where SD(Y) = ", sd_str, " in the\n",
    "  # control group (Rule 3).\n",
    "  declare_model(\n",
    "    N = ", N_str, ",\n",
    "    U = rnorm(N),\n",
    "    potential_outcomes(Y ~ ", tau_str, " * Z + ", sd_str, " * U)\n",
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

  if (tau_from_mde) {
    cat("\n")
    cat(wrap_sentence(
      paste0("Note: Using tau = ", fmt(tau),
             " (MDE at 80% power). Override with ",
             "write_DeclareDesign(result, tau = ...).")
    ), "\n")
  }

  if (sd_backcalc) {
    cat("\n")
    cat(wrap_sentence(
      paste0("Note: The DeclareDesign code uses a back-calculated SD ",
             "(from the predicted SE). For a more natural mapping, use ",
             "from_sd() as the source.")
    ), "\n")
  }

  cat("\n")
  code_lines <- strsplit(code, "\n")[[1]]
  for (line in code_lines) {
    cat("  ", line, "\n", sep = "")
  }

  cat("\n")
  cat(rule_line("Note on power definition"), "\n")
  cat("\n")
  cat(wrap_sentence(paste0(
    "DeclareDesign does not easily support one-sided tests, so ",
    "power above uses two-sided tests. This closely matches the ",
    "directional power from powerrules (Rule 2) ",
    "above about 20% power. Below that, the two-sided power will ",
    "be somewhat higher because it counts significant results in ",
    "the wrong direction as well."
  )), "\n")

  cat("\n")
  cat(rule_line("Reference"), "\n")
  cat("\n")
  cat(wrap_sentence(paste0(
    "See Declaration 18.1 in Blair, Coppock, and Humphreys (2023), ",
    "\"Research Design in the Social Sciences,\" Princeton University ",
    "Press, Chapter 18."
  )), "\n")

  invisible(code)
}
