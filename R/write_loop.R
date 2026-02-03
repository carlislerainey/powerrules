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

  # --- Validation ---
  if (!inherits(result, "power_result"))
    stop("`result` must be a power_result object ",
         "(use find_mde(), find_power(), or find_n())")

  if (result$source$interaction)
    stop("write_loop() does not yet support interaction designs. ",
         "This feature is planned for a future version.")

  if (!is.null(tau)) {
    if (!is.numeric(tau) || length(tau) != 1 || is.na(tau) || tau <= 0)
      stop("`tau` must be a positive number")
  }

  ci <- result$ci
  if (!(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95")

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

  # --- sd_y determination ---
  source <- result$source
  n_planned <- result$n_planned
  sd_backcalc <- FALSE

  if (source$type == "sd") {
    if (source$r_squared > 0) {
      sd_y <- source$sd_y * sqrt(1 - source$r_squared)
      sd_comment <- paste0("   # effective SD: sd_y * sqrt(1 - r_squared) = ",
                           fmt(sd_y))
    } else {
      sd_y <- source$sd_y
      sd_comment <- "   # SD(Y) in control group"
    }
  } else {
    sd_y <- result$se * sqrt(n_planned / 2)
    sd_backcalc <- TRUE
    sd_comment <- "  # back-calculated from predicted SE"
  }

  # --- Format values ---
  n_str <- fmt_val(n_planned)
  tau_str <- if (tau_from_mde) fmt(tau) else fmt_val(tau)
  sd_str <- if (source$type == "sd" && source$r_squared == 0) {
    fmt_val(sd_y)
  } else {
    fmt(sd_y)
  }

  # --- Determine expected power comment ---
  if (result$goal == "mde") {
    expected_comment <- "# Expected: ~80%"
  } else {
    expected_pct <- round(result$power * 100)
    expected_comment <- paste0("# Expected: ~", expected_pct, "%")
  }

  # Tau comment depends on whether tau was defaulted from MDE
  tau_comment <- if (tau_from_mde) {
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
  test_size <- if (ci == 0.90) "0.05" else "0.025"
  test_comment <- paste0("# One-sided p-value and size-", test_size, " test")

  # --- Build code string ---
  code <- paste0(
    "# For-loop power simulation\n",
    "# Confirms the analytical power calculation via simulation.\n",
    "\n",
    "library(sandwich)  # for HC2 robust SEs\n",
    "\n",
    "# --- Parameters ---\n",
    "n    <- ", n_str, n_comment, "\n",
    "tau  <- ", tau_str, tau_comment, "\n",
    "sd_y <- ", sd_str, sd_comment, "\n",
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
    "  reject[i] <- p_val < ", test_size, "\n",
    "}\n",
    "\n",
    "cat(\"Simulated power:\", round(mean(reject) * 100), \"%\\n\")\n",
    expected_comment)

  # --- Print formatted output ---
  cat(rule_line("For-Loop Simulation"), "\n")

  if (tau_from_mde) {
    cat("\n")
    cat(wrap_sentence(
      paste0("Note: Using tau = ", fmt(tau),
             " (MDE at 80% power). Override with ",
             "write_loop(result, tau = ...).")
    ), "\n")
  }

  if (sd_backcalc) {
    cat("\n")
    cat(wrap_sentence(
      paste0("Note: The simulation uses a back-calculated SD ",
             "(from the predicted SE). For a more natural mapping, use ",
             "from_sd() as the source.")
    ), "\n")
  }

  cat("\n")
  code_lines <- strsplit(code, "\n")[[1]]
  for (line in code_lines) {
    cat("  ", line, "\n", sep = "")
  }

  invisible(code)
}
