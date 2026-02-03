# --- print.power_source() ---------------------------------------------------

#' @export
print.power_source <- function(x, ...) {
  labels <- c(sd = "reference population SD",
              existing = "existing study",
              pilot = "pilot data")
  cat("Power source:", labels[x$type], "\n")

  if (x$type == "sd") {
    cat("  SD(Y)       =", fmt_val(x$sd_y), "\n")
    cat("  R^2         =", fmt_val(x$r_squared), "\n")
    cat("  interaction =", x$interaction, "\n")
  } else if (x$type == "existing") {
    cat("  SE (existing) =", fmt_val(x$se_existing), "\n")
    cat("  n (existing)  =", fmt_val(x$n_existing), "\n")
    cat("  interaction   =", x$interaction, "\n")
  } else {
    cat("  SE (pilot)    =", fmt_val(x$se_pilot), "\n")
    cat("  n (pilot)     =", fmt_val(x$n_pilot), "\n")
    cat("  interaction   =", x$interaction, "\n")
  }

  invisible(x)
}

# --- print.power_result() ---------------------------------------------------

#' @export
print.power_result <- function(x, ...) {
  source <- x$source
  interaction <- source$interaction
  n_cond <- if (interaction) 4 else 2
  effect_label <- if (interaction) "interaction" else "treatment effect"
  effect_article <- if (interaction) "an interaction" else "a treatment effect"

  # --- Header block ---
  cat(rule_line("Power Analysis"), "\n")

  design_str <- if (interaction) "balanced, 2x2 factorial (interaction)"
                else "balanced, between-subjects"
  cat("  Design:     ", design_str, "\n", sep = "")

  source_str <- switch(source$type,
    sd = "reference population SD",
    existing = "existing study",
    pilot = "pilot data (conservative)")
  cat("  Source:     ", source_str, "\n", sep = "")

  test_size <- if (x$ci == 0.90) "0.05" else "0.025"
  cat("  CI level:   ", fmt_pct(x$ci), " (size-", test_size,
      " test of directional hypothesis)\n", sep = "")

  cat("\n")

  # --- Inputs ---
  cat("  Inputs:\n")
  print_inputs(x, effect_label, n_cond)

  cat("\n")

  # --- Computation ---
  print_computation(x, effect_label, n_cond)

  cat("\n")

  # --- Manuscript sentence ---
  cat(rule_line("Manuscript sentence (edit as needed)"), "\n")
  sentence <- build_sentence(x, effect_label, effect_article, n_cond)
  cat(wrap_sentence(sentence), "\n")

  # --- Note about MDE factors ---
  note <- build_note(x)
  if (!is.null(note)) {
    cat("\n")
    cat(wrap_sentence(note), "\n")
  }

  invisible(x)
}

# --- Inputs section ----------------------------------------------------------

print_inputs <- function(x, effect_label, n_cond) {
  source <- x$source

  if (source$type == "sd") {
    cat("    SD(Y) =", fmt_val(source$sd_y), "\n")
    if (source$r_squared > 0)
      cat("    R^2   =", fmt_val(source$r_squared), "\n")
  } else if (source$type == "existing") {
    cat("    SE (existing) =", fmt_val(source$se_existing), "\n")
    cat("    n (existing)  =", fmt_val(source$n_existing), "per condition\n")
  } else {
    cat("    SE (pilot) =", fmt_val(source$se_pilot), "\n")
    cat("    n (pilot)  =", fmt_val(source$n_pilot), "per condition\n")
  }

  if (x$goal %in% c("mde", "power")) {
    total <- n_cond * x$n_planned
    n_label <- if (source$type %in% c("existing", "pilot")) "n (planned)   ="
               else "n     ="
    cat(paste0("    ", n_label, " ", fmt_val(x$n_planned),
               " per condition (", fmt_int(total), " total)\n"))
  }

  if (x$goal %in% c("power", "n")) {
    suffix <- if (source$interaction) paste0(" (", effect_label, ")") else ""
    cat(paste0("    tau   = ", fmt_val(x$tau), suffix, "\n"))
  }

  if (x$goal == "n") {
    cat("    power =", fmt_pct(x$power), "\n")
  }
}

# --- Computation section -----------------------------------------------------

print_computation <- function(x, effect_label, n_cond) {
  source <- x$source

  if (x$goal %in% c("mde", "power")) {
    # SE line
    cat(format_se_line(x))

    if (x$goal == "mde") {
      line80 <- paste0("  MDE (80% power) = ", fmt(x$mde_factor_80),
                        " * ", fmt(x$se), " = ", fmt(x$mde_80))
      cat(pad_rule(line80, "[Rule 5]"), "\n")

      line95 <- paste0("  MDE (95% power) = ", fmt(x$mde_factor_95),
                        " * ", fmt(x$se), " = ", fmt(x$mde_95))
      cat(pad_rule(line95, "[Rule 5]"), "\n")

    } else {
      # find_power
      cat("  tau / SE     = ", fmt_val(x$tau), " / ", fmt(x$se),
          " = ", fmt(x$ratio), "\n", sep = "")
      pline <- paste0("  Power        = 1 - pnorm(", fmt(x$critical_value),
                      " - ", fmt(x$ratio), ") = ", fmt_pct(x$power))
      cat(pad_rule(pline, "[Rule 2]"), "\n")
    }

  } else {
    # find_n

    # Conservative factor (pilot only)
    if (source$type == "pilot") {
      cf_inner <- if (source$interaction) {
        paste0("1 / (2 * ", fmt_val(source$n_pilot), ")")
      } else {
        paste0("1 / ", fmt_val(source$n_pilot))
      }
      rule_star <- if (source$interaction) "[Rule 9*]" else "[Rule 9]"
      cfline <- paste0("  Conservative factor = sqrt(", cf_inner,
                       ") + 1 = ", fmt(x$conservative_factor))
      cat(pad_rule(cfline, rule_star), "\n")
    }

    # MDE factor
    ci_arg <- 1 - (1 - x$ci) / 2
    mfline <- paste0("  MDE factor          = qnorm(", fmt_val(ci_arg),
                     ") + qnorm(", fmt(x$power), ") = ",
                     fmt(x$mde_factor_val))
    cat(pad_rule(mfline, "[Table 2]"), "\n")

    # n formula
    cat(format_n_formula(x))

    # n result
    total <- n_cond * x$n_planned
    cond_note <- if (n_cond == 4) ", 4 conditions" else ""
    result_line <- paste0("                      = ", fmt_int(x$n_planned),
                          " per condition (", fmt_int(total), " total",
                          cond_note, ")")
    cat(pad_rule(result_line, format_n_rule(x)), "\n")
  }
}

# Format the SE computation line for find_mde / find_power
format_se_line <- function(x) {
  source <- x$source
  se_str <- fmt(x$se)

  if (source$type == "sd") {
    has_adj <- source$r_squared > 0
    is_int <- source$interaction
    adj <- if (has_adj) paste0(" * sqrt(1 - ", fmt_val(source$r_squared), ")") else ""
    denom <- if (is_int) paste0("sqrt(", fmt_val(x$n_planned), ")")
             else paste0("sqrt(2 * ", fmt_val(x$n_planned), ")")
    formula <- paste0("2 * ", fmt_val(source$sd_y), adj, " / ", denom)

    if (has_adj && is_int) rules <- "[Rules 3-4*]"
    else if (has_adj) rules <- "[Rules 3-4]"
    else if (is_int) rules <- "[Rule 3*]"
    else rules <- "[Rule 3]"

    paste0(pad_rule(paste0("  Predicted SE = ", formula, " = ", se_str),
                    rules), "\n")

  } else if (source$type == "existing") {
    formula <- paste0("sqrt(", fmt_val(source$n_existing), " / ",
                      fmt_val(x$n_planned), ") * ",
                      fmt_val(source$se_existing))
    paste0(pad_rule(paste0("  Predicted SE = ", formula, " = ", se_str),
                    "[Rule 7]"), "\n")

  } else {
    is_int <- source$interaction
    cf_inner <- if (is_int) paste0("1/(2*", fmt_val(source$n_pilot), ")")
                else paste0("1/", fmt_val(source$n_pilot))
    formula <- paste0("sqrt(", fmt_val(source$n_pilot), " / ",
                      fmt_val(x$n_planned), ") * (sqrt(", cf_inner,
                      ") + 1) * ", fmt_val(source$se_pilot))
    rule <- if (is_int) "[Rule 9*]" else "[Rule 9]"
    paste0(pad_rule(paste0("  Predicted SE = ", formula, " = ", se_str),
                    rule), "\n")
  }
}

# Format the n formula line for find_n
format_n_formula <- function(x) {
  source <- x$source
  mf <- fmt(x$mde_factor_val)
  tau <- fmt_val(x$tau)

  if (source$type == "sd") {
    k <- if (source$interaction) "4" else "2"
    has_adj <- source$r_squared > 0
    adj <- if (has_adj) paste0(" * sqrt(1 - ", fmt_val(source$r_squared), ")") else ""
    paste0("  n (planned)         = ", k, " * (", mf, " * ",
           fmt_val(source$sd_y), adj, " / ", tau, ")^2\n")

  } else if (source$type == "existing") {
    paste0("  n (planned)         = ", fmt_val(source$n_existing),
           " * [", mf, " / ", tau, " * ",
           fmt_val(source$se_existing), "]^2\n")

  } else {
    paste0("  n (planned)         = ", fmt_val(source$n_pilot),
           " * [", mf, " / ", tau, " * ",
           fmt(x$conservative_factor), " * ",
           fmt_val(source$se_pilot), "]^2\n")
  }
}

# Format the rule reference for find_n
format_n_rule <- function(x) {
  source <- x$source
  rule_num <- switch(source$type, sd = "6", existing = "8", pilot = "10")
  if (source$interaction) paste0("[Rule ", rule_num, "*]")
  else paste0("[Rule ", rule_num, "]")
}
