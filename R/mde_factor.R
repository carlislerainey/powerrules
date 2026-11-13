# The MDE factor: the multiplier that converts an SE to an MDE (internal)
mde_factor <- function(power, ci = 0.90) {
  qnorm(1 - (1 - ci) / 2) + qnorm(power)
}
