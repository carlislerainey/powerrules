# --- Internal helpers --------------------------------------------------------

# Resolve n_planned from either the n or n_planned argument
resolve_n <- function(n_planned, n) {
  has_np <- !is.null(n_planned)
  has_n  <- !is.null(n)
  if (has_np && has_n)
    stop("specify either `n` or `n_planned`, not both")
  if (!has_np && !has_n)
    stop("must specify sample size per condition (`n` or `n_planned`)")
  if (has_n) n else n_planned
}

# Compute predicted SE from a power_source and planned n per condition
compute_se <- function(source, n_planned) {
  switch(source$type,
    sd = {
      denom <- if (source$interaction) sqrt(n_planned) else sqrt(2 * n_planned)
      2 * source$sd_y * sqrt(1 - source$r_squared) / denom
    },
    existing = {
      sqrt(source$n_existing / n_planned) * source$se_existing
    },
    pilot = {
      cf <- get_conservative_factor(source)
      sqrt(source$n_pilot / n_planned) * cf * source$se_pilot
    },
    stop("unknown source type: ", source$type)
  )
}

# Conservative adjustment factor for pilot data (Rule 9)
get_conservative_factor <- function(source) {
  if (source$type != "pilot") return(NULL)
  cf_denom <- if (source$interaction) 2 * source$n_pilot else source$n_pilot
  sqrt(1 / cf_denom) + 1
}

# --- Goal functions ---------------------------------------------------------

#' Find the minimum detectable effect
#'
#' Given a power source and planned sample size, computes the MDE at both
#' 80% and 95% power (Rule 5 from Rainey, 2026).
#'
#' @param source A `"power_source"` object from `from_sd()`,
#'   `from_existing()`, or `from_pilot()`.
#' @param n_planned Respondents per condition in the planned study. Use
#'   `n_planned` when the source is `from_existing()` or `from_pilot()`;
#'   use `n` when the source is `from_sd()`.
#' @param ci Confidence interval level: 0.90 (default) or 0.95.
#' @param n Alias for `n_planned`. Specify one or the other, not both.
#' @return A `"power_result"` object (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_mde(n = 500)
find_mde <- function(source, n_planned = NULL, ci = 0.90, n = NULL) {
  if (!inherits(source, "power_source"))
    stop("`source` must be a power_source object ",
         "(use from_sd(), from_existing(), or from_pilot())")
  n_planned <- resolve_n(n_planned, n)
  if (!is.numeric(n_planned) || length(n_planned) != 1 ||
      is.na(n_planned) || n_planned <= 0)
    stop("`n_planned` must be a positive number")
  if (!(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95")

  se <- compute_se(source, n_planned)
  mf80 <- mde_factor(0.80, ci)
  mf95 <- mde_factor(0.95, ci)
  mde_80 <- mf80 * se
  mde_95 <- mf95 * se
  cf <- get_conservative_factor(source)

  result <- structure(list(
    goal = "mde",
    source = source,
    ci = ci,
    n_planned = n_planned,
    se = se,
    mde_80 = mde_80,
    mde_95 = mde_95,
    mde_factor_80 = mf80,
    mde_factor_95 = mf95,
    conservative_factor = cf
  ), class = "power_result")

  print(result)
  invisible(result)
}

#' Find the power for a given effect
#'
#' Given a power source, planned sample size, and assumed treatment effect,
#' computes the power (Rule 2 from Rainey, 2026).
#'
#' @param source A `"power_source"` object.
#' @param n_planned Respondents per condition in the planned study. Use
#'   `n_planned` when the source is `from_existing()` or `from_pilot()`;
#'   use `n` when the source is `from_sd()`.
#' @param tau The assumed treatment effect (positive).
#' @param ci Confidence interval level: 0.90 (default) or 0.95.
#' @param n Alias for `n_planned`. Specify one or the other, not both.
#' @return A `"power_result"` object (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)
find_power <- function(source, n_planned = NULL, tau, ci = 0.90, n = NULL) {
  if (!inherits(source, "power_source"))
    stop("`source` must be a power_source object ",
         "(use from_sd(), from_existing(), or from_pilot())")
  n_planned <- resolve_n(n_planned, n)
  if (!is.numeric(n_planned) || length(n_planned) != 1 ||
      is.na(n_planned) || n_planned <= 0)
    stop("`n_planned` must be a positive number")
  if (!is.numeric(tau) || length(tau) != 1 || is.na(tau))
    stop("`tau` must be a number")
  if (tau <= 0)
    stop("`tau` must be positive (use a positive value; ",
         "the sign indicates the direction of the hypothesis)")
  if (!(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95")

  se <- compute_se(source, n_planned)
  cv <- qnorm(1 - (1 - ci) / 2)
  ratio <- tau / se
  power_val <- 1 - pnorm(cv - ratio)
  cf <- get_conservative_factor(source)

  result <- structure(list(
    goal = "power",
    source = source,
    ci = ci,
    n_planned = n_planned,
    tau = tau,
    se = se,
    ratio = ratio,
    critical_value = cv,
    power = power_val,
    conservative_factor = cf
  ), class = "power_result")

  print(result)
  invisible(result)
}

#' Find the required sample size
#'
#' Given a power source, assumed treatment effect, and desired power level,
#' computes the required sample size per condition (Rules 6/8/10 from
#' Rainey, 2026).
#'
#' @param source A `"power_source"` object.
#' @param tau The assumed treatment effect (positive).
#' @param power Desired power level (default 0.80).
#' @param ci Confidence interval level: 0.90 (default) or 0.95.
#' @return A `"power_result"` object (returned invisibly).
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_n(tau = 3)
find_n <- function(source, tau, power = 0.80, ci = 0.90) {
  if (!inherits(source, "power_source"))
    stop("`source` must be a power_source object ",
         "(use from_sd(), from_existing(), or from_pilot())")
  if (!is.numeric(tau) || length(tau) != 1 || is.na(tau))
    stop("`tau` must be a number")
  if (tau <= 0)
    stop("`tau` must be positive (use a positive value; ",
         "the sign indicates the direction of the hypothesis)")
  if (!is.numeric(power) || length(power) != 1 ||
      is.na(power) || power <= 0 || power >= 1)
    stop("`power` must be between 0 and 1 (exclusive)")
  if (!(ci %in% c(0.90, 0.95)))
    stop("`ci` must be 0.90 or 0.95")

  mf <- mde_factor(power, ci)
  interaction <- source$interaction
  cf <- get_conservative_factor(source)

  n_raw <- switch(source$type,
    sd = {
      k <- if (interaction) 4 else 2
      k * (mf * source$sd_y * sqrt(1 - source$r_squared) / tau)^2
    },
    existing = {
      source$n_existing * (mf * source$se_existing / tau)^2
    },
    pilot = {
      source$n_pilot * (mf * cf * source$se_pilot / tau)^2
    },
    stop("unknown source type: ", source$type)
  )

  n_planned <- ceiling(n_raw)
  se <- compute_se(source, n_planned)

  result <- structure(list(
    goal = "n",
    source = source,
    ci = ci,
    tau = tau,
    power = power,
    n_planned = n_planned,
    n_planned_raw = n_raw,
    se = se,
    mde_factor_val = mf,
    conservative_factor = cf
  ), class = "power_result")

  print(result)
  invisible(result)
}
