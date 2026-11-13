# --- Manual calculation code generator ----------------------------------------

#' Generate manual calculation code for a power analysis
#'
#' Prints copy-pasteable base R code that reproduces the power calculation
#' using the paper's rounded constants and critical values. Because the
#' pipeline uses exact values, the generated code can give slightly different
#' results (for example, a required sample size a few respondents larger).
#'
#' @param result A `"power_result"` object from `find_mde()`,
#'   `find_power()`, or `find_n()`.
#' @return The generated code as a character string (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_mde(n = 500) |> write_manual()
write_manual <- function(result) {
  check_result(result, "write_manual")

  k <- paper_constants(result$ci)
  code <- build_manual_code(result, k)

  cat(rule_line("Manual Calculation"), "\n")
  if (!uses_rounded_constants(result))
    print_note("Note: Using qnorm() for the MDE factor because power = ",
               fmt_val(result$power * 100), "% does not match the paper's ",
               "rounded constants (80% \u2192 ", k$mde_80, ", 95% \u2192 ",
               k$mde_95, ").")
  print_code(code)

  invisible(code)
}

# The generated code uses the paper's rounded constants unless find_n() was
# asked for a power level the paper gives no rounded factor for
uses_rounded_constants <- function(result) {
  result$goal != "n" || is_standard_power(result$power)
}

# --- Internal: build the manual calculation code string ----------------------

# `k` holds the paper's rounded constants for the result's CI level
build_manual_code <- function(result, k) {
  source <- result$source
  goal <- result$goal

  # Whether the source introduces its own sample size
  has_source_n <- source$type %in% c("existing", "pilot")
  n_var <- if (has_source_n) "n_planned" else "n"

  # --- Collect input variable assignments ---
  # Each entry: c(name, value, comment)
  input_vars <- switch(source$type,
    sd = c(
      list(c("sd_y", fmt_val(source$sd_y),
             "SD of outcome in reference population")),
      if (source$r_squared > 0)
        list(c("r_squared", fmt_val(source$r_squared),
               "R-squared of control variables"))
    ),
    existing = list(
      c("se_existing", fmt_val(source$se_existing), "SE from existing study"),
      c("n_existing", fmt_val(source$n_existing),
        "n per condition in existing study")
    ),
    pilot = list(
      c("se_pilot", fmt_val(source$se_pilot), "SE from pilot study"),
      c("n_pilot", fmt_val(source$n_pilot), "n per condition in pilot study")
    )
  )

  if (goal %in% c("mde", "power")) {
    n_label <- if (has_source_n) "n per condition in planned study"
               else "respondents per condition"
    input_vars <- c(input_vars,
                    list(c(n_var, fmt_val(result$n_planned), n_label)))
  }

  if (goal %in% c("power", "n")) {
    input_vars <- c(input_vars,
                    list(c("tau", fmt_val(result$tau),
                           "assumed treatment effect")))
  }

  # Say so in the code when it uses the rounded constants, since the pipeline
  # itself uses exact values
  header <- "# Manual power calculation\n"
  if (uses_rounded_constants(result)) {
    header <- paste0(header,
      "# Uses the paper's rounded constants, so results can differ slightly\n",
      "# from the powerrules pipeline, which uses exact values.\n")
  }

  # Format with aligned <- operators
  input_names <- vapply(input_vars, `[`, character(1), 1)
  input_lines <- vapply(input_vars, function(v) {
    paste0(formatC(v[1], width = -max(nchar(input_names))),
           " <- ", v[2], "   # ", v[3], "\n")
  }, character(1))
  inputs <- paste0(header, "\n# --- Inputs ---\n",
                   paste(input_lines, collapse = ""))

  # --- Computation block ---
  computation <- switch(goal,
    mde = build_manual_mde(source, n_var, k),
    power = build_manual_power(source, n_var, k),
    n = build_manual_n(result, k)
  )

  paste0(inputs, computation)
}

# --- find_mde code ---

build_manual_mde <- function(source, n_var, k) {
  paste0(build_se_block(source, n_var),
    "\n# --- MDE (Rule 5) ---\n",
    "mde_80 <- ", k$mde_80, " * se   # 80% power\n",
    "mde_95 <- ", k$mde_95, " * se   # 95% power\n",
    "\n",
    "cat(\"Predicted SE:      \", round(se, 2), \"\\n\")\n",
    "cat(\"MDE (80% power):   \", round(mde_80, 2), \"\\n\")\n",
    "cat(\"MDE (95% power):   \", round(mde_95, 2), \"\\n\")")
}

# --- find_power code ---

build_manual_power <- function(source, n_var, k) {
  paste0(build_se_block(source, n_var),
    "\n# --- Power (Rule 2) ---\n",
    "power <- 1 - pnorm(", k$cv, " - tau / se)\n",
    "\n",
    "cat(\"Predicted SE:        \", round(se, 2), \"\\n\")\n",
    "cat(\"tau / SE:            \", round(tau / se, 2), \"\\n\")\n",
    "cat(\"Power:               \", round(power * 100), \"%\\n\")")
}

# --- find_n code ---

build_manual_n <- function(result, k) {
  source <- result$source
  power <- result$power
  standard_power <- is_standard_power(power)
  is_80 <- is_power_level(power, 0.80)

  code <- ""

  if (source$type == "pilot") {
    code <- paste0(code,
      "\n# --- Conservative factor (Pilot Rule 3) ---\n",
      "cf <- sqrt(1 / n_pilot) + 1\n")
  }

  if (!standard_power) {
    code <- paste0(code,
      "\n# --- MDE factor ---\n",
      "mde_factor <- qnorm(", fmt_val(1 - test_size(result$ci)),
      ") + qnorm(", fmt_val(power), ")  # MDE factor for ",
      fmt_val(power * 100), "% power\n")
  }

  # The rounded constant, or the qnorm() fallback computed above
  mf_str <- if (!standard_power) "mde_factor"
            else if (is_80) k$mde_80
            else k$mde_95

  rule_label <- switch(source$type, sd = "Rule 6",
                       existing = "Pilot Rule 2", pilot = "Pilot Rule 4")
  n_formula <- switch(source$type,
    sd = paste0("2 * (", mf_str, " * sd_y",
                if (source$r_squared > 0) " * sqrt(1 - r_squared)",
                " / tau)^2"),
    existing = paste0("n_existing * (", mf_str, " * se_existing / tau)^2"),
    pilot = paste0("n_pilot * (", mf_str, " * cf * se_pilot / tau)^2")
  )

  paste0(code,
    "\n# --- Required n per condition (", rule_label, ") ---\n",
    "n <- ceiling(", n_formula, ")",
    if (standard_power) paste0("   # ", if (is_80) "80%" else "95%", " power"),
    "\n",
    "\n",
    "cat(\"Required n/condition: \", n, \"\\n\")\n",
    "cat(\"Total N:              \", 2 * n, \"\\n\")")
}

# --- SE block builder (shared by mde and power) ---

build_se_block <- function(source, n_var) {
  switch(source$type,
    sd = {
      has_adj <- source$r_squared > 0
      paste0(
        "\n# --- Predicted SE (", if (has_adj) "Rules 3-4" else "Rule 3",
        ") ---\n",
        "se <- 2 * sd_y", if (has_adj) " * sqrt(1 - r_squared)",
        " / sqrt(2 * ", n_var, ")\n")
    },
    existing = paste0(
      "\n# --- Predicted SE (Pilot Rule 1) ---\n",
      "se <- sqrt(n_existing / ", n_var, ") * se_existing\n"),
    pilot = paste0(
      "\n# --- Conservative factor (Pilot Rule 3) ---\n",
      "cf <- sqrt(1 / n_pilot) + 1\n",
      "\n# --- Predicted SE (Pilot Rule 3) ---\n",
      "se <- sqrt(n_pilot / ", n_var, ") * cf * se_pilot\n")
  )
}
