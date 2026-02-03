# --- Manuscript sentence -----------------------------------------------------

#' Build a manuscript sentence from a power result
#'
#' Constructs a publication-ready sentence describing the power analysis.
#' Used internally by the print method and the Shiny app.
#'
#' @param x A `"power_result"` object.
#' @param effect_label "treatment effect" or "interaction".
#' @param effect_article "a treatment effect" or "an interaction".
#' @param n_cond Number of conditions (2 or 4).
#' @return A character string.
#' @keywords internal
#' @export
build_sentence <- function(x, effect_label, effect_article, n_cond) {
  if (x$goal == "n") {
    build_sentence_n(x, effect_label, effect_article, n_cond)
  } else {
    build_sentence_se(x, effect_label, effect_article, n_cond)
  }
}

# Sentence for find_mde / find_power (SE-based goals)
build_sentence_se <- function(x, effect_label, effect_article, n_cond) {
  source <- x$source
  interaction <- source$interaction
  total <- n_cond * x$n_planned
  total_str <- fmt_int(total)
  n_str <- fmt_int(x$n_planned)

  design_str <- if (interaction) "balanced, 2x2 factorial"
                else "balanced, between-subjects"
  cond_note <- if (n_cond == 4) paste0(total_str, " total, 4 conditions")
               else paste0(total_str, " total")

  opening <- paste0("For a ", design_str, " design with ", n_str,
                    " respondents per condition (", cond_note, ")")

  # Source description
  src_desc <- switch(source$type,
    sd = {
      d <- paste0(", assuming a standard deviation of ", fmt_val(source$sd_y))
      if (source$r_squared > 0)
        d <- paste0(d, " and control variables that explain ",
                    round(source$r_squared * 100),
                    "% of the variance in the outcome")
      d
    },
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

  se_part <- paste0(", the predicted standard error is ", fmt(x$se), ".")

  # Conclusion
  test_size <- if (x$ci == 0.90) "0.05" else "0.025"
  if (x$goal == "mde") {
    conclusion <- paste0(
      " Using a one-sided test at the ", test_size, " level, the experiment has ",
      "80% power to detect ", effect_article, " of ", fmt(x$mde_80),
      " units and 95% power to detect ", effect_article, " of ", fmt(x$mde_95),
      " units."
    )
  } else {
    power_pct <- round(x$power * 100)
    conclusion <- paste0(
      " Using a one-sided test at the ", test_size, " level, the experiment has ",
      power_pct, "% power to detect ", effect_article, " of ",
      fmt_val(x$tau), " units."
    )
  }

  paste0(opening, src_desc, se_part, conclusion)
}

# Sentence for find_n
build_sentence_n <- function(x, effect_label, effect_article, n_cond) {
  source <- x$source
  interaction <- source$interaction
  total <- n_cond * x$n_planned
  total_str <- fmt_int(total)
  n_str <- fmt_int(x$n_planned)
  power_pct <- round(x$power * 100)
  test_size <- if (x$ci == 0.90) "0.05" else "0.025"

  design_str <- if (interaction) "balanced, 2x2 factorial"
                else "balanced, between-subjects"

  cond_note <- if (n_cond == 4) ", 4 conditions" else ""

  if (source$type == "sd") {
    src_desc <- paste0(", assuming a standard deviation of ",
                       fmt_val(source$sd_y))
    if (source$r_squared > 0)
      src_desc <- paste0(src_desc, " and control variables that explain ",
                         round(source$r_squared * 100),
                         "% of the variance in the outcome")

    cond_note_str <- if (n_cond == 4) paste0(total_str, " total, 4 conditions")
                     else paste0(total_str, " total")
    paste0(
      "For a ", design_str, " design", src_desc,
      ", the experiment requires ", n_str,
      " respondents per condition (", cond_note_str, ") for ",
      power_pct, "% power to detect ",
      effect_article, " of ", fmt_val(x$tau),
      " units, using a one-sided test at the ", test_size, " level."
    )

  } else if (source$type == "existing") {
    paste0(
      "For a ", design_str, " design replicating an existing study ",
      "with a standard error of ", fmt_val(source$se_existing),
      " (", fmt_val(source$n_existing), " per condition), ",
      "the experiment requires ", n_str,
      " respondents per condition (", total_str, " total", cond_note, ") for ",
      power_pct, "% power to detect ", effect_article, " of ",
      fmt_val(x$tau),
      " units, using a one-sided test at the ", test_size, " level."
    )

  } else {
    paste0(
      "For a ", design_str, " design, using pilot data with a standard ",
      "error of ", fmt_val(source$se_pilot), " (",
      fmt_val(source$n_pilot),
      " per condition) and a conservative adjustment for pilot noise, ",
      "the experiment requires ", n_str,
      " respondents per condition (", total_str, " total", cond_note,
      ") for ", power_pct, "% power to detect ", effect_article, " of ",
      fmt_val(x$tau),
      " units, using a one-sided test at the ", test_size, " level."
    )
  }
}

# --- MDE factor note ---------------------------------------------------------

#' Build the MDE factor note for a power result
#'
#' Explains the difference between exact and rounded MDE factors.
#' Used internally by the print method and the Shiny app.
#'
#' @param x A `"power_result"` object.
#' @return A character string, or NULL for find_power results.
#' @keywords internal
#' @export
build_note <- function(x) {
  if (x$goal == "power") return(NULL)

  # Rounded constants depend on CI level (90% CI: 2.5/3.3; 95% CI: 2.8/3.6)
  r80 <- if (x$ci == 0.95) "2.8" else "2.5"
  r95 <- if (x$ci == 0.95) "3.6" else "3.3"

  if (x$goal == "mde") {
    paste0(
      "Note: The paper rounds the MDE factor to ", r80, " for 80% power ",
      "and ", r95, " for 95% power. This software uses exact values (", fmt(x$mde_factor_80),
      " and ", fmt(x$mde_factor_95),
      "), so results differ slightly from hand calculations using the ",
      "rounded factors."
    )
  } else {
    power_pct <- round(x$power * 100)
    rounded <- if (power_pct == 80) r80
               else if (power_pct == 95) r95
               else NULL

    if (!is.null(rounded)) {
      paste0(
        "Note: The paper rounds the MDE factor to ", rounded,
        " for ", power_pct, "% power. This software uses the exact value (",
        fmt(x$mde_factor_val),
        "), so results differ slightly from hand calculations using the ",
        "rounded factor."
      )
    } else {
      paste0(
        "Note: The paper rounds the MDE factor to ", r80, " for 80% power ",
        "and ", r95, " for 95% power. This software uses the exact MDE factor (",
        fmt(x$mde_factor_val),
        "), so results may differ slightly from hand calculations using ",
        "the rounded factors."
      )
    }
  }
}
