# --- Input validation helpers (internal) -------------------------------------
# Each stops with a message naming the offending argument. `call. = FALSE`
# keeps these helpers' own names out of the error the user sees.

check_positive <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0)
    stop("`", name, "` must be a positive number", call. = FALSE)
}

check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x))
    stop("`", name, "` must be TRUE or FALSE", call. = FALSE)
}

check_source <- function(source) {
  if (!inherits(source, "power_source"))
    stop("`source` must be a power_source object ",
         "(use from_sd(), from_existing(), or from_pilot())", call. = FALSE)
}

# The planned sample size per condition
check_n_planned <- function(n_planned) {
  check_positive(n_planned, "n_planned")
  if (abs(n_planned - round(n_planned)) > 1e-8)
    stop("`n_planned` must be a whole number of respondents per condition",
         call. = FALSE)
}

# The assumed treatment effect for find_power() and find_n()
check_tau <- function(tau) {
  if (!is.numeric(tau) || length(tau) != 1 || is.na(tau))
    stop("`tau` must be a number", call. = FALSE)
  if (tau <= 0)
    stop("`tau` must be positive (use a positive value; ",
         "the sign indicates the direction of the hypothesis)", call. = FALSE)
}

check_ci <- function(ci) {
  if (!is.numeric(ci) || length(ci) != 1 || is.na(ci) ||
      !(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95", call. = FALSE)
}

# The result handed to a write_*() function; `fn` is that function's name
check_result <- function(result, fn) {
  if (!inherits(result, "power_result"))
    stop("`result` must be a power_result object ",
         "(use find_mde(), find_power(), or find_n())", call. = FALSE)
  if (result$source$interaction)
    stop(fn, "() does not yet support interaction designs. ",
         "This feature is planned for a future version.", call. = FALSE)
}
