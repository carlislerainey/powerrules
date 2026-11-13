# --- Manuscript sentence (internal) -------------------------------------------

# A publication-ready sentence describing the power analysis. Shared by the
# print method and the Shiny app.
build_sentence <- function(x) {
  source <- x$source
  interaction <- source$interaction

  design <- if (interaction) "balanced, 2x2 factorial"
            else "balanced, between-subjects"
  effect <- if (interaction) "an interaction" else "a treatment effect"
  sizes <- paste0(
    fmt_int(x$n_planned), " respondents per condition (",
    fmt_int(n_conditions(source) * x$n_planned), " total",
    if (interaction) ", 4 conditions", ")"
  )
  test <- paste0("a one-sided test at the ", fmt_val(test_size(x$ci)), " level")

  src_desc <- switch(source$type,
    sd = paste0(
      ", assuming a standard deviation of ", fmt_val(source$sd_y),
      if (source$r_squared > 0)
        paste0(" and control variables that explain ",
               fmt_val(source$r_squared * 100),
               "% of the variance in the outcome")
    ),
    existing = paste0(
      " replicating an existing study with a standard error of ",
      fmt_val(source$se_existing), " (", fmt_val(source$n_existing),
      " per condition)"
    ),
    pilot = paste0(
      ", using pilot data with a standard error of ",
      fmt_val(source$se_pilot), " (", fmt_val(source$n_pilot),
      " per condition) and a conservative adjustment for pilot noise"
    )
  )

  if (x$goal == "n") {
    return(paste0(
      "For a ", design, " design", src_desc, ", the experiment requires ",
      sizes, " for ", fmt_val(x$power * 100), "% power to detect ", effect,
      " of ", fmt_val(x$tau), " units, using ", test, "."
    ))
  }

  finding <- if (x$goal == "mde") {
    paste0("80% power to detect ", effect, " of ", fmt(x$mde_80),
           " units and 95% power to detect ", effect, " of ", fmt(x$mde_95),
           " units.")
  } else {
    power_str <- if (x$power > 0.995) "more than 99%"
                 else paste0(round(x$power * 100), "%")
    paste0(power_str, " power to detect ", effect, " of ", fmt_val(x$tau),
           " units.")
  }

  paste0(
    "For a ", design, " design with ", sizes, src_desc,
    ", the predicted standard error is ", fmt(x$se), ". Using ", test,
    ", the experiment has ", finding
  )
}

# --- MDE factor note (internal) -----------------------------------------------

# Explains the difference between the exact MDE factors and the paper's
# rounded ones. NULL for find_power results, which use no MDE factor.
build_note <- function(x) {
  if (x$goal == "power") return(NULL)

  k <- paper_constants(x$ci)
  both <- paste0(
    "Note: The paper rounds the MDE factor to ", k$mde_80, " for 80% power ",
    "and ", k$mde_95, " for 95% power. This software uses "
  )

  if (x$goal == "mde") {
    paste0(
      both, "exact values (", fmt(x$mde_factor_80), " and ",
      fmt(x$mde_factor_95),
      "), so results differ slightly from hand calculations using the ",
      "rounded factors."
    )
  } else if (is_standard_power(x$power)) {
    rounded <- if (is_power_level(x$power, 0.80)) k$mde_80 else k$mde_95
    paste0(
      "Note: The paper rounds the MDE factor to ", rounded, " for ",
      fmt_val(x$power * 100), "% power. This software uses the exact value (",
      fmt(x$mde_factor_val),
      "), so results differ slightly from hand calculations using the ",
      "rounded factor."
    )
  } else {
    paste0(
      both, "the exact MDE factor (", fmt(x$mde_factor_val),
      "), so results may differ slightly from hand calculations using ",
      "the rounded factors."
    )
  }
}
