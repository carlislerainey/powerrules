# helpers.R — Rendering functions for the Power Rules Shiny app
# Produces color-coded HTML paralleling print.R's console output.

# --- Package internals ------------------------------------------------------
# The app ships inside powerrules, so it shares the package's formatters and
# text builders with the console output rather than keeping copies of them.

fmt                 <- powerrules:::fmt
fmt_val             <- powerrules:::fmt_val
fmt_int             <- powerrules:::fmt_int
fmt_prob            <- powerrules:::fmt_prob
fmt_power           <- powerrules:::fmt_power
mde_factor          <- powerrules:::mde_factor
mde_factor_table    <- powerrules:::mde_factor_table
format_n_rule       <- powerrules:::format_n_rule
build_sentence      <- powerrules:::build_sentence
build_note          <- powerrules:::build_note
build_pipeline_code <- powerrules:::build_pipeline_code

# --- HTML span wrappers ----------------------------------------------------

vi <- function(x) paste0('<span class="val-input">', x, '</span>')
vr <- function(x) paste0('<span class="val-result">', x, '</span>')
vb <- function() '<span class="val-blank">&nbsp;</span>'
rr <- function(text) paste0('<span class="rule-ref">', text, '</span>')

# A single formula line: formula text left, rule reference right
fline <- function(formula_html, rule_text = NULL) {
  rule <- if (!is.null(rule_text) && nzchar(rule_text)) rr(rule_text) else ""
  paste0('<div class="formula-line"><span class="formula-text">',
         formula_html, '</span>', rule, '</div>')
}

# --- SE line rendering ------------------------------------------------------

render_se_line <- function(source_type, interaction, inputs, use_r2, result) {
  has_result <- !is.null(result)
  se_val <- if (has_result) vr(fmt(result$se)) else vb()

  if (source_type == "sd") {
    sd_y <- if (!is.null(inputs$sd_y)) vi(fmt_val(inputs$sd_y)) else vb()
    n_val <- if (!is.null(inputs$n_planned)) vi(fmt_val(inputs$n_planned)) else vb()

    r2_part <- ""
    if (use_r2) {
      r2_str <- if (!is.null(inputs$r_squared)) vi(fmt_val(inputs$r_squared)) else vb()
      r2_part <- paste0(" \u00d7 sqrt(1 \u2212 ", r2_str, ")")
    }

    denom <- if (interaction) {
      paste0("sqrt(", n_val, ")")
    } else {
      paste0("sqrt(2 \u00d7 ", n_val, ")")
    }

    formula <- paste0("Predicted SE = 2 \u00d7 ", sd_y, r2_part, " / ", denom,
                       " = ", se_val)

    rule <- if (use_r2 && interaction) "[Rules 3-4*]"
    else if (use_r2) "[Rules 3-4]"
    else if (interaction) "[Rule 3*]"
    else "[Rule 3]"

    fline(formula, rule)

  } else if (source_type == "existing") {
    n_exist <- if (!is.null(inputs$n_existing)) vi(fmt_val(inputs$n_existing)) else vb()
    n_plan  <- if (!is.null(inputs$n_planned))  vi(fmt_val(inputs$n_planned))  else vb()
    se_exist <- if (!is.null(inputs$se_existing)) vi(fmt_val(inputs$se_existing)) else vb()

    formula <- paste0("Predicted SE = sqrt(", n_exist, " / ", n_plan,
                       ") \u00d7 ", se_exist, " = ", se_val)
    fline(formula, "[Pilot Rule 1]")

  } else { # pilot
    n_pil  <- if (!is.null(inputs$n_pilot))  vi(fmt_val(inputs$n_pilot))  else vb()
    n_plan <- if (!is.null(inputs$n_planned)) vi(fmt_val(inputs$n_planned)) else vb()
    se_pil <- if (!is.null(inputs$se_pilot)) vi(fmt_val(inputs$se_pilot)) else vb()

    cf_inner <- if (interaction) {
      paste0("1/(2\u00d7", n_pil, ")")
    } else {
      paste0("1/", n_pil)
    }

    formula <- paste0("Predicted SE = sqrt(", n_pil, " / ", n_plan,
                       ") \u00d7 (sqrt(", cf_inner, ") + 1) \u00d7 ",
                       se_pil, " = ", se_val)
    rule <- if (interaction) "[Pilot Rule 3*]" else "[Pilot Rule 3]"
    fline(formula, rule)
  }
}

# --- MDE lines (find_mde) --------------------------------------------------

render_mde_lines <- function(ci, result) {
  mf80 <- fmt(mde_factor(0.80, ci))
  mf95 <- fmt(mde_factor(0.95, ci))
  has_result <- !is.null(result)

  se_str  <- if (has_result) vr(fmt(result$se))     else vb()
  mde80   <- if (has_result) vr(fmt(result$mde_80)) else vb()
  mde95   <- if (has_result) vr(fmt(result$mde_95)) else vb()

  line80 <- paste0("MDE (80% power) = ", mf80, " \u00d7 ", se_str, " = ", mde80)
  line95 <- paste0("MDE (95% power) = ", mf95, " \u00d7 ", se_str, " = ", mde95)

  paste(fline(line80, "[Rule 5]"), fline(line95, "[Rule 5]"), sep = "\n")
}

# --- Power lines (find_power) ----------------------------------------------

render_power_lines <- function(ci, inputs, result) {
  cv <- fmt(qnorm(1 - (1 - ci) / 2))
  tau_str <- if (!is.null(inputs$tau)) vi(fmt_val(inputs$tau)) else vb()
  has_result <- !is.null(result)

  se_str    <- if (has_result) vr(fmt(result$se))         else vb()
  ratio_str <- if (has_result) vr(fmt(result$ratio))      else vb()
  power_str <- if (has_result) vr(fmt_power(result$power)) else vb()

  line1 <- paste0("tau / SE = ", tau_str, " / ", se_str, " = ", ratio_str)
  line2 <- paste0("Power = 1 \u2212 pnorm(", cv, " \u2212 ", ratio_str,
                   ") = ", power_str)

  paste(fline(line1), fline(line2, "[Rule 2]"), sep = "\n")
}

# --- Conservative factor line (find_n, pilot only) -------------------------

render_cf_line <- function(interaction, inputs, result) {
  n_pil <- if (!is.null(inputs$n_pilot)) vi(fmt_val(inputs$n_pilot)) else vb()
  cf_val <- if (!is.null(result)) vr(fmt(result$conservative_factor)) else vb()

  inner <- if (interaction) {
    paste0("1 / (2 \u00d7 ", n_pil, ")")
  } else {
    paste0("1 / ", n_pil)
  }

  rule <- if (interaction) "[Pilot Rule 3*]" else "[Pilot Rule 3]"
  fline(paste0("Conservative factor = sqrt(", inner, ") + 1 = ", cf_val), rule)
}

# --- MDE factor line (find_n) ----------------------------------------------

render_mf_line <- function(ci, power_level, result) {
  ci_arg  <- fmt_prob(1 - (1 - ci) / 2)
  pow_arg <- fmt_prob(power_level)
  mf_val  <- if (!is.null(result)) vr(fmt(result$mde_factor_val)) else vb()

  fline(paste0("MDE factor = qnorm(", ci_arg, ") + qnorm(", pow_arg,
               ") = ", mf_val), mde_factor_table(power_level, ci))
}

# --- Sample size formula lines (find_n) ------------------------------------

render_n_lines <- function(source_type, interaction, inputs, use_r2, result) {
  has_result <- !is.null(result)
  tau_str <- if (!is.null(inputs$tau)) vi(fmt_val(inputs$tau)) else vb()
  mf_str  <- if (has_result) vr(fmt(result$mde_factor_val)) else vb()

  if (source_type == "sd") {
    sd_y <- if (!is.null(inputs$sd_y)) vi(fmt_val(inputs$sd_y)) else vb()
    k <- if (interaction) "4" else "2"
    r2_part <- ""
    if (use_r2) {
      r2_str <- if (!is.null(inputs$r_squared)) vi(fmt_val(inputs$r_squared)) else vb()
      r2_part <- paste0(" \u00d7 sqrt(1 \u2212 ", r2_str, ")")
    }
    formula <- paste0("n (planned) = ", k, " \u00d7 (", mf_str, " \u00d7 ",
                       sd_y, r2_part, " / ", tau_str, ")\u00b2")

  } else if (source_type == "existing") {
    n_exist  <- if (!is.null(inputs$n_existing))  vi(fmt_val(inputs$n_existing))  else vb()
    se_exist <- if (!is.null(inputs$se_existing)) vi(fmt_val(inputs$se_existing)) else vb()
    formula <- paste0("n (planned) = ", n_exist, " \u00d7 [", mf_str,
                       " / ", tau_str, " \u00d7 ", se_exist, "]\u00b2")

  } else { # pilot
    n_pil  <- if (!is.null(inputs$n_pilot))  vi(fmt_val(inputs$n_pilot))  else vb()
    se_pil <- if (!is.null(inputs$se_pilot)) vi(fmt_val(inputs$se_pilot)) else vb()
    cf_str <- if (has_result) vr(fmt(result$conservative_factor)) else vb()
    formula <- paste0("n (planned) = ", n_pil, " \u00d7 [", mf_str,
                       " / ", tau_str, " \u00d7 ", cf_str, " \u00d7 ",
                       se_pil, "]\u00b2")
  }

  lines <- fline(formula)

  # Result line
  if (has_result) {
    n_cond   <- if (interaction) 4 else 2
    total    <- n_cond * result$n_planned
    n_str    <- vr(fmt_int(result$n_planned))
    total_str <- vr(fmt_int(total))
    cond_note <- if (n_cond == 4) ", 4 conditions" else ""

    result_html <- paste0(
      '<span class="formula-continuation">= ', n_str,
      ' per condition (', total_str, ' total', cond_note, ')</span>')
    lines <- paste(lines, fline(result_html, format_n_rule(result)), sep = "\n")
  }

  lines
}

# --- Main computation renderer ---------------------------------------------

render_computation <- function(source_type, goal_type, interaction, ci,
                               power_level, inputs, use_r2, result) {
  lines <- character()

  if (goal_type %in% c("mde", "power")) {
    lines <- c(lines, render_se_line(source_type, interaction, inputs,
                                      use_r2, result))
    if (goal_type == "mde") {
      lines <- c(lines, render_mde_lines(ci, result))
    } else {
      lines <- c(lines, render_power_lines(ci, inputs, result))
    }
  } else { # find_n
    if (source_type == "pilot") {
      lines <- c(lines, render_cf_line(interaction, inputs, result))
    }
    lines <- c(lines, render_mf_line(ci, power_level, result))
    lines <- c(lines, render_n_lines(source_type, interaction, inputs,
                                      use_r2, result))
  }

  paste(lines, collapse = "\n")
}

# --- Result highlight -------------------------------------------------------

render_result_highlight <- function(result, goal_type, interaction) {
  if (is.null(result)) return("")

  n_cond <- if (interaction) 4 else 2

  if (goal_type == "mde") {
    paste0('<div class="result-highlight">',
           'MDE (80% power) = <span class="result-value-red">',
           fmt(result$mde_80), '</span>',
           ' &nbsp;|&nbsp; ',
           'MDE (95% power) = <span class="result-value-blue">',
           fmt(result$mde_95), '</span></div>')

  } else if (goal_type == "power") {
    paste0('<div class="result-highlight">',
           'Power = <span class="result-value-red">',
           fmt_power(result$power), '</span></div>')

  } else {
    total <- n_cond * result$n_planned
    cond_note <- if (n_cond == 4) ", 4 conditions" else ""
    paste0('<div class="result-highlight">',
           '<span class="result-value-red">', fmt_int(result$n_planned),
           '</span> per condition (<span class="result-value-red">',
           fmt_int(total), '</span> total', cond_note, ')</div>')
  }
}

# --- Manuscript sentence ----------------------------------------------------

render_sentence <- function(result) {
  if (is.null(result)) return("")

  sentence <- build_sentence(result)

  paste0('<div class="sentence-box">',
         '<div class="sentence-header">Manuscript sentence (edit as needed)</div>',
         '<div class="sentence-text" id="sentence-text">',
         htmltools::htmlEscape(sentence), '</div>',
         '<button class="copy-btn" onclick="copyText(\'sentence-text\')">',
         'Copy</button></div>')
}

# --- MDE factor note --------------------------------------------------------

render_note <- function(result) {
  if (is.null(result)) return("")

  note <- build_note(result)
  if (is.null(note)) return("")

  paste0('<div class="note-box">', htmltools::htmlEscape(note), '</div>')
}

# --- Pipeline code ----------------------------------------------------------

render_pipeline <- function(source_type, goal_type, interaction, ci,
                            power_level, inputs) {
  pipe_inputs <- inputs
  pipe_inputs$interaction <- interaction
  pipe_inputs$ci <- ci
  pipe_inputs$power <- power_level

  pipeline <- build_pipeline_code(source_type, goal_type, pipe_inputs)

  # a script that runs on its own: install, load, compute
  goal_label <- switch(goal_type, mde = "MDE", power = "power",
                       n = "sample size")
  script <- paste0(
    "# install package\n",
    "pak::pak(\"carlislerainey/powerrules\")\n\n",
    "# load package\n",
    "library(powerrules)\n\n",
    "# compute ", goal_label, "\n",
    pipeline
  )

  paste0('<div class="pipeline-box">',
         '<div class="pipeline-header">Save this to your script</div>',
         '<code class="pipeline-code" id="pipeline-text">',
         htmltools::htmlEscape(script), '</code>',
         '<button class="copy-btn" onclick="copyText(\'pipeline-text\')">',
         'Copy</button></div>')
}
