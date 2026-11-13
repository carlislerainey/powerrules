# --- For-loop simulation code generator ---------------------------------------

#' Generate for-loop simulation code for a power analysis
#'
#' Prints copy-pasteable R code for a for-loop simulation that confirms the
#' analytical power calculation. Uses potential outcomes with a constant
#' additive treatment effect and HC2 robust standard errors (via the
#' \pkg{sandwich} package).
#'
#' @param result A `"power_result"` object from `find_mde()`,
#'   `find_power()`, or `find_n()`.
#' @param tau Effect size for the simulation. Defaults to `result$tau`
#'   for `find_power`/`find_n` results, or `result$mde_80`
#'   for `find_mde` results (with a note).
#' @return The generated code as a character string (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3) |> write_loop()
write_loop <- function(result, tau = NULL) {

  check_result(result, "write_loop")
  sim <- sim_setup(result, tau)
  source <- result$source
  ci <- result$ci

  # --- Comment on where the SD comes from ---
  sd_comment <- if (sim$sd_backcalc) {
    "  # back-calculated from predicted SE"
  } else if (source$r_squared > 0) {
    paste0("   # effective SD: sd_y * sqrt(1 - r_squared) = ", sim$sd_str)
  } else {
    "   # SD(Y) in control group"
  }

  # --- Determine expected power comment ---
  # Computed from the simulated tau and the predicted SE (Rule 2), so the
  # comment stays right when the caller overrides tau
  expected_power <- 1 - pnorm(qnorm(1 - (1 - ci) / 2) - sim$tau / result$se)
  expected_comment <- paste0("# Expected: ~", round(expected_power * 100), "%")

  # Tau comment depends on whether tau was defaulted from MDE
  tau_comment <- if (sim$tau_from_mde) {
    "   # treatment effect (MDE at 80% power)"
  } else {
    "      # treatment effect"
  }

  # --- n comment ---
  if (result$goal == "n") {
    n_comment <- "   # respondents per condition (computed by powerrules)"
  } else {
    n_comment <- "    # respondents per condition"
  }

  # --- Test size ---
  size <- fmt_val(test_size(ci))
  test_comment <- paste0("# One-sided p-value and size-", size, " test")

  # --- Build code string ---
  code <- paste0(
    "# For-loop power simulation\n",
    "# Confirms the analytical power calculation via simulation.\n",
    "\n",
    "library(sandwich)  # for HC2 robust SEs\n",
    "\n",
    "# --- Parameters ---\n",
    "n    <- ", fmt_val(result$n_planned), n_comment, "\n",
    "tau  <- ", sim$tau_str, tau_comment, "\n",
    "sd_y <- ", sim$sd_str, sd_comment, "\n",
    "\n",
    "# --- Simulate 500 experiments ---\n",
    "n_sims <- 500\n",
    "set.seed(1234)\n",
    "reject <- logical(n_sims)\n",
    "\n",
    "for (i in seq_len(n_sims)) {\n",
    "  # Potential outcomes: constant, additive treatment effect\n",
    "  y0 <- rnorm(2 * n, mean = 0, sd = sd_y)\n",
    "  y1 <- y0 + tau\n",
    "\n",
    "  # Complete random assignment: n to each condition\n",
    "  z <- sample(rep(0:1, each = n))\n",
    "\n",
    "  # Reveal observed outcome\n",
    "  y <- ifelse(z == 1, y1, y0)\n",
    "\n",
    "  # Estimate via OLS with HC2 robust SEs\n",
    "  fit <- lm(y ~ z)\n",
    "  est <- coef(fit)[\"z\"]\n",
    "  se  <- sqrt(vcovHC(fit, type = \"HC2\")[\"z\", \"z\"])\n",
    "\n",
    "  ", test_comment, "\n",
    "  p_val <- pnorm(est / se, lower.tail = FALSE)\n",
    "  reject[i] <- p_val < ", size, "\n",
    "}\n",
    "\n",
    "cat(\"Simulated power:\", round(mean(reject) * 100), \"%\\n\")\n",
    expected_comment)

  # --- Print formatted output ---
  cat(rule_line("For-Loop Simulation"), "\n")
  print_sim_notes(sim, "write_loop", "simulation")
  print_code(code)

  invisible(code)
}
