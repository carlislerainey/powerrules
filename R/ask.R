# --- Interactive input helpers (internal) ------------------------------------
# Small wrappers around readline() for the interactive guide.

# Guard for interactive session; mockable in tests
check_interactive <- function() {
  if (!interactive())
    stop("power_rules() requires an interactive R session")
}

# Ask a menu question and return the selected key
ask_menu <- function(prompt, choices) {
  cat("\n")
  cat(prompt, "\n\n")
  keys <- names(choices)
  for (k in keys) {
    cat("  (", k, ") ", choices[[k]], "\n", sep = "")
  }
  cat("\n")

  repeat {
    answer <- trimws(readline("Your choice: "))
    if (answer %in% keys) return(answer)
    cat("  Please enter one of: ", paste(keys, collapse = ", "), "\n", sep = "")
  }
}

# Ask for a number until `valid()` accepts it; `retry` is the reminder shown
# after an answer it rejects
ask_number <- function(prompt, hint = NULL, valid = function(val) val > 0,
                       retry = "Please enter a positive number.") {
  cat("\n")
  cat(prompt, "\n")
  if (!is.null(hint)) cat("  (", hint, ")\n", sep = "")
  cat("\n")

  repeat {
    answer <- trimws(readline("Value: "))
    val <- suppressWarnings(as.numeric(answer))
    if (!is.na(val) && valid(val)) return(val)
    cat("  ", retry, "\n", sep = "")
  }
}

# Ask for an R-squared value in [0, 1)
ask_r_squared <- function(prompt, hint = NULL) {
  ask_number(prompt, hint, valid = function(val) val >= 0 && val < 1,
             retry = "Please enter a number between 0 and 1 (exclusive).")
}

# Ask for a power level (strictly between 0 and 1)
ask_power <- function(prompt, hint = NULL) {
  ask_number(prompt, hint, valid = function(val) val > 0 && val < 1,
             retry = "Please enter a number between 0 and 1 (exclusive).")
}

# Print a section divider
ask_divider <- function() {
  cat("\n", paste(rep("-", 60), collapse = ""), "\n", sep = "")
}
