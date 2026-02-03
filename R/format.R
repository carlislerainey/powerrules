# --- Formatting helpers (internal) -------------------------------------------

# Computed values: fixed 2 decimal places
fmt <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

# Input values: natural display (no trailing zeros)
fmt_val <- function(x) {
  if (abs(x - round(x)) < 1e-10) {
    formatC(round(x), format = "f", digits = 0)
  } else {
    s <- formatC(x, format = "f", digits = 6)
    s <- sub("0+$", "", s)
    sub("\\.$", "", s)
  }
}

# Integer with comma separator
fmt_int <- function(x) {
  format(ceiling(x), big.mark = ",")
}

# Proportion to percentage string
fmt_pct <- function(x) {
  paste0(round(x * 100), "%")
}

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
