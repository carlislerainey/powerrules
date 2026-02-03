# Verify that find_power() and write_DeclareDesign() agree.
#
# DeclareDesign uses a two-sided test (p <= alpha), while find_power()
# uses the one-sided power formula from Rule 2: 1 - pnorm(cv - tau/se).
# The two-sided power is:
#   (1 - pnorm(cv - tau/se)) + pnorm(-cv - tau/se)
#
# The second term, pnorm(-cv - tau/se), is the probability of rejecting
# in the wrong direction. It is negligible for moderate/high power but
# grows as power approaches the test size. For 90% CIs (cv = 1.645):
#
#   power ≈ 30%  →  divergence ≈ 0.003
#   power ≈ 20%  →  divergence ≈ 0.007
#   power ≈ 15%  →  divergence ≈ 0.012
#   power ≈ 10%  →  divergence ≈ 0.022
#   power ≈  5%  →  divergence ≈ 0.050

quiet <- function(expr) invisible(capture.output(expr))

# Exact two-sided power (what DeclareDesign converges to as sims → ∞)
twosided_power <- function(se, tau, ci = 0.90) {
  cv <- qnorm(1 - (1 - ci) / 2)
  ratio <- tau / se
  (1 - pnorm(cv - ratio)) + pnorm(-cv - ratio)
}


# --- Divergence at low power (deterministic) --------------------------------

test_that("divergence at low power equals pnorm(-cv - tau/se)", {
  n_planned <- 200
  ci <- 0.90
  cv <- qnorm(1 - (1 - ci) / 2)

  cases <- list(
    list(sd_y = 50,  tau = 0.5),
    list(sd_y = 50,  tau = 1.0),
    list(sd_y = 100, tau = 1.0)
  )

  for (case in cases) {
    quiet(res <- from_sd(sd_y = case$sd_y) |>
      find_power(n_planned = n_planned, tau = case$tau, ci = ci))

    expect_true(res$power < 0.15,
      label = sprintf("sd_y=%g, tau=%g should give low power (got %.3f)",
        case$sd_y, case$tau, res$power))

    power_twosided <- twosided_power(res$se, case$tau, ci)
    wrong_tail <- pnorm(-cv - case$tau / res$se)

    # Two-sided power exceeds one-sided
    expect_true(power_twosided > res$power)

    # The gap is exactly the wrong-tail probability
    expect_equal(power_twosided - res$power, wrong_tail, tolerance = 1e-10)

    # The wrong-tail probability is non-negligible at low power
    expect_true(wrong_tail > 0.01,
      label = sprintf("wrong_tail=%.4f for sd_y=%g, tau=%g",
        wrong_tail, case$sd_y, case$tau))
  }
})
