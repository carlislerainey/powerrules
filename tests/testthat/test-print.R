# Printed output of sources and results. `printed()` collapses the printed
# lines with newlines; `flat()` also squishes whitespace so phrases that the
# manuscript sentence wraps across lines can be matched.

printed <- function(res) paste(capture.output(print(res)), collapse = "\n")
flat <- function(res) gsub("\\s+", " ", printed(res))

# --- Sources ------------------------------------------------------------------

test_that("print.power_source() outputs source type and stored values", {
  out <- paste(capture.output(print(from_sd(sd_y = 20.8, r_squared = 0.05))),
               collapse = "\n")
  expect_match(out, "reference population SD")
  expect_match(out, "20.8", fixed = TRUE)
  expect_match(out, "0.05", fixed = TRUE)

  out2 <- paste(capture.output(
    print(from_existing(se_existing = 1.8, n_existing = 268))),
    collapse = "\n")
  expect_match(out2, "existing study")

  out3 <- paste(capture.output(
    print(from_pilot(se_pilot = 2.0, n_pilot = 50))), collapse = "\n")
  expect_match(out3, "pilot data")
})

# --- find_mde ----------------------------------------------------------------

test_that("print of from_sd |> find_mde shows design, source, CI, MDE lines", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- printed(res)
  fl <- flat(res)
  expect_match(out, "between-subjects")
  expect_match(out, "reference population SD")
  expect_match(out, "90%")
  expect_match(out, "size-0.05")
  expect_match(out, "MDE (80% power) = ", fixed = TRUE)
  expect_match(out, "MDE (95% power) = ", fixed = TRUE)
  expect_match(out, "Manuscript sentence")
  expect_no_match(out, "Rainey")
  expect_match(out, "Note:")
  expect_match(out, "rounds the MDE factor")
  expect_match(fl, "treatment effect", fixed = TRUE)
  expect_match(fl, "1,000 total", fixed = TRUE)
  # No R^2 line when r_squared = 0
  expect_no_match(out, "R^2", fixed = TRUE)
  # No output cites the old combined paper's Rules 7-10
  expect_no_match(out, "\\[Rule (7|8|9|10)\\*?\\]")
})

test_that("print of find_mde shows an R^2 input line only when r_squared > 0", {
  with_r2 <- from_sd(sd_y = 20.8, r_squared = 0.05) |> find_mde(n = 500)
  expect_match(printed(with_r2), "R^2   = 0.05", fixed = TRUE)
  zero_r2 <- from_sd(sd_y = 20.8, r_squared = 0) |> find_mde(n = 500)
  expect_no_match(printed(zero_r2), "R^2", fixed = TRUE)
})

test_that("print of find_mde shows the 95% CI level line", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95)
  out <- printed(res)
  expect_match(out, "95%")
  expect_match(out, "size-0.025")
})

test_that("print of pilot find_mde shows the source line", {
  res <- from_pilot(se_pilot = 2.0, n_pilot = 50) |> find_mde(n_planned = 500)
  expect_match(printed(res), "pilot data")
})

test_that("print of existing + interaction find_mde shows 2x2 and 4n total", {
  res <- from_existing(se_existing = 1.8, n_existing = 268,
                       interaction = TRUE) |>
    find_mde(n_planned = 500)
  expect_match(printed(res), "2x2 factorial", fixed = TRUE)
  expect_match(flat(res), "2,000 total", fixed = TRUE)
})

test_that("sd interaction find_mde sentence reports the four conditions", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |> find_mde(n = 375)
  expect_match(build_sentence(res), ", 4 conditions)", fixed = TRUE)
})

test_that("find_mde with n = 50000 never uses scientific notation", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n = 50000)
  expect_match(flat(res), "100,000 total", fixed = TRUE)
  expect_no_match(flat(res), "e+", fixed = TRUE)
})

# --- find_n ------------------------------------------------------------------

test_that("print of sd interaction find_n shows interaction design and total", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  expect_match(printed(res), "2x2 factorial", fixed = TRUE)
  expect_match(flat(res), "interaction", fixed = TRUE)
  expect_match(flat(res), "4 conditions", fixed = TRUE)
  expect_match(build_sentence(res), ", 4 conditions)", fixed = TRUE)
})

test_that("print of sd find_n has the MDE factor note and one 'per condition'", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3)
  out <- printed(res)
  expect_match(out, "Note:")
  # Default power: two decimals kept in the MDE factor line
  expect_match(out, "qnorm(0.95) + qnorm(0.80)", fixed = TRUE)

  res95 <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  out95 <- capture.output(print(res95))
  sentence <- paste(out95[grep("Manuscript sentence", out95):length(out95)],
                    collapse = " ")
  matches <- gregexpr("respondents per condition", sentence)[[1]]
  expect_equal(sum(matches > 0), 1)
})

test_that("print of pilot find_n says 'conservative'", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  expect_match(flat(res), "conservative", ignore.case = TRUE)
})

test_that("find_n with ci = 0.95 displays qnorm(0.975), not qnorm(0.97)", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, ci = 0.95)
  expect_match(printed(res), "qnorm(0.975)", fixed = TRUE)
  expect_no_match(printed(res), "qnorm(0.97)", fixed = TRUE)
})

# --- find_power --------------------------------------------------------------

test_that("print of find_power has no MDE factor note", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  expect_no_match(printed(res), "Note:")
})

# --- Power levels are displayed without rounding away digits ----------------

test_that("find_n displays a 99.9% power request exactly, never as 100%", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.999)
  out <- printed(res)
  expect_match(out, "qnorm(0.999)", fixed = TRUE)
  expect_match(out, "power = 99.9%", fixed = TRUE)
  expect_match(out, "99.9% power", fixed = TRUE)
  expect_no_match(out, "qnorm(1.00)", fixed = TRUE)
  expect_no_match(out, "100%", fixed = TRUE)
})

test_that("find_n displays a 97.5% power request consistently", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.975)
  out <- printed(res)
  expect_match(out, "qnorm(0.95) + qnorm(0.975)", fixed = TRUE)
  expect_match(out, "power = 97.5%", fixed = TRUE)
  expect_match(out, "97.5% power", fixed = TRUE)
})

test_that("find_power never reports 100% power", {
  res <- from_sd(sd_y = 20.8) |> find_power(n = 5000, tau = 5)
  expect_match(flat(res), "= >99%", fixed = TRUE)
  expect_match(flat(res), "more than 99% power", fixed = TRUE)
  expect_no_match(flat(res), "100%", fixed = TRUE)

  # Ordinary power levels are unchanged
  res2 <- from_sd(sd_y = 20.8) |> find_power(n = 500, tau = 3)
  expect_match(flat(res2), "= 74%", fixed = TRUE)
  expect_match(flat(res2), "has 74% power", fixed = TRUE)
})

# --- MDE factor note ---------------------------------------------------------

test_that("the MDE factor note keys on the exact power level", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.804)
  expect_match(build_note(res), "2.5 for 80% power and 3.3 for 95% power",
               fixed = TRUE)
  res80 <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80)
  expect_match(build_note(res80), "to 2.5 for 80% power. This", fixed = TRUE)
})

test_that("the MDE factor note is singular for find_n and plural for find_mde", {
  res_n <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.80)
  expect_match(build_note(res_n), "rounded factor.", fixed = TRUE)
  expect_match(build_note(res_n), "rounded factor\\.$")
  res_mde <- from_sd(sd_y = 20.8) |> find_mde(n = 500)
  expect_match(build_note(res_mde), "rounded factors\\.$")
})

# --- Number formatting ------------------------------------------------------

test_that("fmt_int never uses scientific notation", {
  expect_equal(fmt_int(100000), "100,000")
  expect_equal(fmt_int(1e6), "1,000,000")
})

test_that("fmt_val keeps small and long inputs", {
  expect_equal(fmt_val(20.8), "20.8")
  expect_equal(fmt_val(500), "500")
  expect_equal(fmt_val(0.975), "0.975")
  expect_equal(fmt_val(0.0000001), "0.0000001")
  expect_equal(fmt_val(0.00000012), "0.00000012")
  expect_equal(fmt_val(1 / 3), "0.3333333333")
  expect_equal(fmt_val(0.1 + 0.2), "0.3")
})

test_that("fmt_pct and fmt_power format proportions", {
  expect_equal(fmt_pct(0.80), "80%")
  expect_equal(fmt_pct(0.90), "90%")
  expect_equal(fmt_pct(0.975), "97.5%")
  expect_equal(fmt_pct(0.999), "99.9%")
  expect_equal(fmt_power(0.7375), "74%")
  expect_equal(fmt_power(0.9951), ">99%")
  expect_equal(fmt_power(1), ">99%")
})

test_that("sentences report R-squared without rounding it away", {
  res <- from_sd(sd_y = 20.8, r_squared = 0.014) |> find_mde(n = 500)
  expect_match(build_sentence(res), "explain 1.4% of the variance",
               fixed = TRUE)
  res2 <- from_sd(sd_y = 20.8, r_squared = 0.05) |>
    find_n(tau = 3, power = 0.95)
  expect_match(build_sentence(res2), "explain 5% of the variance",
               fixed = TRUE)
})

# --- Rule and table references ----------------------------------------------

test_that("existing-study and pilot sources cite the pilot paper's rules", {
  ex <- from_existing(se_existing = 1.8, n_existing = 268)
  out_ex_mde <- printed(find_mde(ex, n_planned = 500))
  out_ex_n <- printed(find_n(ex, tau = 3))
  expect_match(out_ex_mde, "[Pilot Rule 1]", fixed = TRUE)
  expect_match(out_ex_n, "[Pilot Rule 2]", fixed = TRUE)

  pi <- from_pilot(se_pilot = 2.13, n_pilot = 85)
  out_pi_mde <- printed(find_mde(pi, n_planned = 500))
  out_pi_n <- printed(find_n(pi, tau = 3))
  expect_match(out_pi_mde, "[Pilot Rule 3]", fixed = TRUE)
  expect_match(out_pi_n, "[Pilot Rule 3]", fixed = TRUE)
  expect_match(out_pi_n, "[Pilot Rule 4]", fixed = TRUE)

  # Interactions: the pilot rules are modified (starred); the existing-study
  # rules are not
  pi_int <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE)
  expect_match(printed(find_n(pi_int, tau = 0.67)), "[Pilot Rule 4*]",
               fixed = TRUE)
  ex_int <- from_existing(se_existing = 0.18, n_existing = 375,
                          interaction = TRUE)
  expect_match(printed(find_n(ex_int, tau = 0.67)), "[Pilot Rule 2]",
               fixed = TRUE)

  # No output cites the old combined paper's Rules 7-10
  all_out <- paste(out_ex_mde, out_ex_n, out_pi_mde, out_pi_n)
  expect_no_match(all_out, "\\[Rule (7|8|9|10)\\*?\\]")
})

test_that("the MDE factor line cites the right table", {
  s <- from_sd(sd_y = 20.8)
  expect_match(printed(find_n(s, tau = 3, power = 0.80)), "[Table 1]",
               fixed = TRUE)
  expect_match(printed(find_n(s, tau = 3, power = 0.95, ci = 0.95)),
               "[Table 1]", fixed = TRUE)
  expect_match(printed(find_n(s, tau = 3, power = 0.90)), "[Table 3]",
               fixed = TRUE)
  expect_match(printed(find_n(s, tau = 3, power = 0.90, ci = 0.95)),
               "[Table 4]", fixed = TRUE)
})
