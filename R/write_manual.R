# --- Manual calculation code generator ----------------------------------------

#' Generate manual calculation code for a power analysis
#'
#' Prints copy-pasteable base R code that reproduces the power calculation
#' using the paper's rounded constants and critical values.
#'
#' @param result A `"power_result"` object from `find_mde()`,
#'   `find_power()`, or `find_n()`.
#' @return The generated code as a character string (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_mde(n = 500) |> write_manual()
write_manual <- function(result) {

  # --- Validation ---
  if (!inherits(result, "power_result"))
    stop("`result` must be a power_result object ",
         "(use find_mde(), find_power(), or find_n())")

  if (result$source$interaction)
    stop("write_manual() does not yet support interaction designs. ",
         "This feature is planned for a future version.")

  ci <- result$ci
  if (!(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95")

  # --- Constants by CI level ---
  if (ci == 0.90) {
    mde_const_80 <- "2.5"
    mde_const_95 <- "3.3"
    cv_const     <- "1.64"
  } else {
    mde_const_80 <- "2.8"
    mde_const_95 <- "3.6"
    cv_const     <- "1.96"
  }

  source <- result$source
  goal   <- result$goal

  # --- Build code string ---
  code <- build_manual_code(result, source, goal, ci,
                            mde_const_80, mde_const_95, cv_const)

  # --- Determine if qnorm note is needed ---
  qnorm_note <- FALSE
  if (goal == "n") {
    power <- result$power
    if (!(power %in% c(0.80, 0.95))) {
      qnorm_note <- TRUE
    }
  }

  # --- Print formatted output ---
  cat(rule_line("Manual Calculation"), "\n")

  if (qnorm_note) {
    cat("\n")
    if (ci == 0.90) {
      note_constants <- "(80% \u2192 2.5, 95% \u2192 3.3)"
    } else {
      note_constants <- "(80% \u2192 2.8, 95% \u2192 3.6)"
    }
    cat(wrap_sentence(
      paste0("Note: Using qnorm() for the MDE factor because power = ",
             round(power * 100), "% does not match the paper's rounded ",
             "constants ", note_constants, ".")
    ), "\n")
  }

  cat("\n")
  code_lines <- strsplit(code, "\n")[[1]]
  for (line in code_lines) {
    cat("  ", line, "\n", sep = "")
  }

  invisible(code)
}

# --- Internal: build the manual calculation code string ----------------------

build_manual_code <- function(result, source, goal, ci,
                              mde_const_80, mde_const_95, cv_const) {

  # Whether the source introduces its own sample size
  has_source_n <- source$type %in% c("existing", "pilot")
  n_var <- if (has_source_n) "n_planned" else "n"

  # --- Collect input variable assignments ---
  # Each entry: list(name, value_str, comment)
  input_vars <- list()

  if (source$type == "sd") {
    input_vars <- c(input_vars, list(
      list("sd_y", fmt_val(source$sd_y), "SD of outcome in reference population")))
    if (source$r_squared > 0) {
      input_vars <- c(input_vars, list(
        list("r_squared", fmt_val(source$r_squared), "R-squared of control variables")))
    }
  } else if (source$type == "existing") {
    input_vars <- c(input_vars, list(
      list("se_existing", fmt_val(source$se_existing), "SE from existing study"),
      list("n_existing", fmt_val(source$n_existing), "n per condition in existing study")))
  } else if (source$type == "pilot") {
    input_vars <- c(input_vars, list(
      list("se_pilot", fmt_val(source$se_pilot), "SE from pilot study"),
      list("n_pilot", fmt_val(source$n_pilot), "n per condition in pilot study")))
  }

  if (goal %in% c("mde", "power")) {
    n_label <- if (has_source_n) "n per condition in planned study" else "respondents per condition"
    input_vars <- c(input_vars, list(
      list(n_var, fmt_val(result$n_planned), n_label)))
  }

  if (goal %in% c("power", "n")) {
    input_vars <- c(input_vars, list(
      list("tau", fmt_val(result$tau), "assumed treatment effect")))
  }

  # Format with aligned <- operators
  max_name_len <- max(vapply(input_vars, function(x) nchar(x[[1]]), integer(1)))
  inputs <- "# Manual power calculation\n\n# --- Inputs ---\n"
  for (v in input_vars) {
    padded_name <- paste0(v[[1]],
      paste(rep(" ", max_name_len - nchar(v[[1]])), collapse = ""))
    inputs <- paste0(inputs,
      padded_name, " <- ", v[[2]], "   # ", v[[3]], "\n")
  }

  # --- Computation block ---
  computation <- ""

  if (goal == "mde") {
    computation <- build_manual_mde(source, n_var, mde_const_80, mde_const_95)
  } else if (goal == "power") {
    computation <- build_manual_power(source, n_var, cv_const)
  } else if (goal == "n") {
    computation <- build_manual_n(result, source, ci,
                                  mde_const_80, mde_const_95)
  }

  paste0(inputs, computation)
}

# --- find_mde code ---

build_manual_mde <- function(source, n_var, mde_const_80, mde_const_95) {
  se_block <- build_se_block(source, n_var)

  paste0(se_block,
    "\n# --- MDE (Rule 5) ---\n",
    "mde_80 <- ", mde_const_80, " * se   # 80% power\n",
    "mde_95 <- ", mde_const_95, " * se   # 95% power\n",
    "\n",
    "cat(\"Predicted SE:      \", round(se, 2), \"\\n\")\n",
    "cat(\"MDE (80% power):   \", round(mde_80, 2), \"\\n\")\n",
    "cat(\"MDE (95% power):   \", round(mde_95, 2), \"\\n\")")
}

# --- find_power code ---

build_manual_power <- function(source, n_var, cv_const) {
  se_block <- build_se_block(source, n_var)

  paste0(se_block,
    "\n# --- Power (Rule 2) ---\n",
    "power <- 1 - pnorm(", cv_const, " - tau / se)\n",
    "\n",
    "cat(\"Predicted SE:        \", round(se, 2), \"\\n\")\n",
    "cat(\"tau / SE:            \", round(tau / se, 2), \"\\n\")\n",
    "cat(\"Power:               \", round(power * 100), \"%\\n\")")
}

# --- find_n code ---

build_manual_n <- function(result, source, ci, mde_const_80, mde_const_95) {
  power <- result$power

  # Determine MDE factor representation
  standard_power <- power %in% c(0.80, 0.95)

  if (standard_power) {
    mde_const <- if (power == 0.80) mde_const_80 else mde_const_95
    power_label <- if (power == 0.80) "80%" else "95%"
  }

  code <- ""

  if (source$type == "pilot") {
    code <- paste0(code,
      "\n# --- Conservative factor (Rule 9) ---\n",
      "cf <- sqrt(1 / n_pilot) + 1\n")
  }

  if (!standard_power) {
    alpha <- 1 - ci
    code <- paste0(code,
      "\n# --- MDE factor ---\n",
      "mde_factor <- qnorm(", fmt_val(1 - alpha / 2), ") + qnorm(",
      fmt_val(power), ")  # MDE factor for ",
      round(power * 100), "% power\n")
  }

  # Build the n formula based on source type
  rule_num <- switch(source$type, sd = "6", existing = "8", pilot = "10")

  code <- paste0(code,
    "\n# --- Required n per condition (Rule ", rule_num, ") ---\n")

  mf_str <- if (standard_power) mde_const else "mde_factor"

  if (source$type == "sd") {
    if (source$r_squared > 0) {
      code <- paste0(code,
        "n <- ceiling(2 * (", mf_str, " * sd_y * sqrt(1 - r_squared) / tau)^2)")
    } else {
      code <- paste0(code,
        "n <- ceiling(2 * (", mf_str, " * sd_y / tau)^2)")
    }
  } else if (source$type == "existing") {
    code <- paste0(code,
      "n <- ceiling(n_existing * (", mf_str, " * se_existing / tau)^2)")
  } else if (source$type == "pilot") {
    code <- paste0(code,
      "n <- ceiling(n_pilot * (", mf_str, " * cf * se_pilot / tau)^2)")
  }

  if (standard_power) {
    code <- paste0(code, "   # ", power_label, " power")
  }

  code <- paste0(code, "\n",
    "\n",
    "cat(\"Required n/condition: \", n, \"\\n\")\n",
    "cat(\"Total N:              \", 2 * n, \"\\n\")")

  code
}

# --- SE block builder (shared by mde and power) ---

build_se_block <- function(source, n_var) {
  if (source$type == "sd") {
    rule <- if (source$r_squared > 0) "Rule 4" else "Rule 3"
    if (source$r_squared > 0) {
      paste0(
        "\n# --- Predicted SE (", rule, ") ---\n",
        "se <- 2 * sd_y * sqrt(1 - r_squared) / sqrt(2 * ", n_var, ")\n")
    } else {
      paste0(
        "\n# --- Predicted SE (", rule, ") ---\n",
        "se <- 2 * sd_y / sqrt(2 * ", n_var, ")\n")
    }
  } else if (source$type == "existing") {
    paste0(
      "\n# --- Predicted SE (Rule 7) ---\n",
      "se <- sqrt(n_existing / ", n_var, ") * se_existing\n")
  } else if (source$type == "pilot") {
    paste0(
      "\n# --- Conservative factor (Rule 9) ---\n",
      "cf <- sqrt(1 / n_pilot) + 1\n",
      "\n# --- Predicted SE (Rule 9) ---\n",
      "se <- sqrt(n_pilot / ", n_var, ") * cf * se_pilot\n")
  }
}
