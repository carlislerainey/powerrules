#' Compute the MDE factor
#'
#' Returns `qnorm(1 - (1 - ci) / 2) + qnorm(power)`, the multiplier
#' that converts SE to MDE.
#'
#' @param power Desired power level (e.g., 0.80 or 0.95).
#' @param ci Confidence interval level (default 0.90).
#' @return A numeric scalar.
#' @keywords internal
mde_factor <- function(power, ci = 0.90) {
  qnorm(1 - (1 - ci) / 2) + qnorm(power)
}
