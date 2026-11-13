# --- Formatting helpers (internal) -------------------------------------------

# Computed values: fixed 2 decimal places
fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

# Input values: natural display (no trailing zeros, up to 10 significant
# digits, never scientific notation)
fmt_val <- function(x) {
  if (abs(x - round(x)) < 1e-10) {
    formatC(round(x), format = "f", digits = 0)
  } else {
    format(x, digits = 10, scientific = FALSE, drop0trailing = TRUE)
  }
}

# Integer with comma separator (never scientific notation)
fmt_int <- function(x) {
  format(ceiling(x), big.mark = ",", scientific = FALSE)
}

# Input proportion to percentage string, without rounding away digits the
# user supplied (0.80 -> "80%", 0.975 -> "97.5%", 0.999 -> "99.9%")
fmt_pct <- function(x) {
  paste0(fmt_val(x * 100), "%")
}

# Probability as a qnorm() argument: two decimals when that is exact
# (0.80, 0.95), otherwise every digit the user supplied (0.975, 0.999)
fmt_prob <- function(x) {
  if (abs(x * 100 - round(x * 100)) < 1e-8) fmt(x) else fmt_val(x)
}

# Computed power to percentage string: whole percent, but never "100%"
fmt_power <- function(x) {
  if (x > 0.995) ">99%" else paste0(round(x * 100), "%")
}

# --- Shared lookups (internal) -----------------------------------------------

# Does a power level equal `level`, allowing for floating-point error?
is_power_level <- function(power, level) {
  abs(power - level) < 1e-8
}

# Is this one of the two power levels the paper gives a rounded factor for?
is_standard_power <- function(power) {
  is_power_level(power, 0.80) || is_power_level(power, 0.95)
}

# Where the paper tabulates the MDE factor: Table 1 for the headline 80% and
# 95% power levels; Appendix Tables 3 (90% CI) and 4 (95% CI) otherwise
mde_factor_table <- function(power, ci) {
  if (is_standard_power(power)) "[Table 1]"
  else if (ci == 0.90) "[Table 3]"
  else "[Table 4]"
}

# The paper's rounded MDE factors (80% and 95% power) and critical value
paper_constants <- function(ci) {
  if (ci == 0.90) list(mde_80 = "2.5", mde_95 = "3.3", cv = "1.64")
  else list(mde_80 = "2.8", mde_95 = "3.6", cv = "1.96")
}

# Size of the one-sided test behind a confidence level (0.05 for a 90% CI)
test_size <- function(ci) {
  (1 - ci) / 2
}

# Number of conditions in the design
n_conditions <- function(source) {
  if (source$interaction) 4 else 2
}

# --- Text layout (internal) ---------------------------------------------------

# Section rule line
rule_line <- function(text) {
  width <- 72
  n_dash <- max(width - nchar(text) - 4, 3)
  paste0("-- ", text, " ", paste(rep("-", n_dash), collapse = ""))
}

# Word-wrap text with indent
wrap_sentence <- function(text, width = 70, indent = 2) {
  prefix <- paste(rep(" ", indent), collapse = "")
  words <- strsplit(text, " ")[[1]]
  lines <- character()
  current <- prefix
  for (w in words) {
    test <- if (nchar(trimws(current)) == 0) paste0(current, w)
            else paste0(current, " ", w)
    if (nchar(test) > width + indent && nchar(trimws(current)) > 0) {
      lines <- c(lines, current)
      current <- paste0(prefix, w)
    } else {
      current <- test
    }
  }
  if (nchar(trimws(current)) > 0) lines <- c(lines, current)
  paste(lines, collapse = "\n")
}

# Right-pad text and append a rule reference at a target column
pad_rule <- function(text, rule_ref, total_width = 72) {
  gap <- max(total_width - nchar(text) - nchar(rule_ref), 2)
  paste0(text, paste(rep(" ", gap), collapse = ""), rule_ref)
}
