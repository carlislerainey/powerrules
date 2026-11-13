# Each case is list(function that should fail, error-message pattern).
expect_all_error <- function(cases) {
  for (nm in names(cases)) {
    expect_error(cases[[nm]][[1]](), cases[[nm]][[2]], info = nm)
  }
}

# --- Constructors ---

test_that("from_sd() rejects bad inputs", {
  expect_all_error(list(
    "sd_y negative"     = list(function() from_sd(sd_y = -1), "sd_y.*positive"),
    "sd_y zero"         = list(function() from_sd(sd_y = 0), "sd_y.*positive"),
    "sd_y NA"           = list(function() from_sd(sd_y = NA_real_), "sd_y.*positive"),
    "sd_y non-numeric"  = list(function() from_sd(sd_y = "a"), "sd_y.*positive"),
    "sd_y wrong length" = list(function() from_sd(sd_y = c(1, 2)), "sd_y.*positive"),
    "r_squared < 0"     = list(function() from_sd(sd_y = 20, r_squared = -0.1), "r_squared"),
    "r_squared = 1"     = list(function() from_sd(sd_y = 20, r_squared = 1), "r_squared"),
    "r_squared > 1"     = list(function() from_sd(sd_y = 20, r_squared = 1.5), "r_squared"),
    "r_squared NA"      = list(function() from_sd(sd_y = 20, r_squared = NA_real_), "r_squared"),
    "interaction char"  = list(function() from_sd(sd_y = 20, interaction = "yes"), "interaction"),
    "interaction NA"    = list(function() from_sd(sd_y = 20, interaction = NA), "interaction")
  ))
})

test_that("from_existing() rejects bad inputs", {
  expect_all_error(list(
    "se negative"      = list(function() from_existing(se_existing = -1, n_existing = 100), "se_existing.*positive"),
    "se zero"          = list(function() from_existing(se_existing = 0, n_existing = 100), "se_existing.*positive"),
    "se NA"            = list(function() from_existing(se_existing = NA_real_, n_existing = 100), "se_existing.*positive"),
    "n negative"       = list(function() from_existing(se_existing = 1.8, n_existing = -10), "n_existing.*positive"),
    "n zero"           = list(function() from_existing(se_existing = 1.8, n_existing = 0), "n_existing.*positive"),
    "n NA"             = list(function() from_existing(se_existing = 1.8, n_existing = NA_real_), "n_existing.*positive"),
    "interaction char" = list(function() from_existing(se_existing = 1.8, n_existing = 268, interaction = "yes"), "interaction"),
    "interaction NA"   = list(function() from_existing(se_existing = 1.8, n_existing = 268, interaction = NA), "interaction")
  ))
})

test_that("from_pilot() rejects bad inputs", {
  expect_all_error(list(
    "se negative"    = list(function() from_pilot(se_pilot = -1, n_pilot = 50), "se_pilot.*positive"),
    "se zero"        = list(function() from_pilot(se_pilot = 0, n_pilot = 50), "se_pilot.*positive"),
    "se NA"          = list(function() from_pilot(se_pilot = NA_real_, n_pilot = 50), "se_pilot.*positive"),
    "n negative"     = list(function() from_pilot(se_pilot = 2, n_pilot = -5), "n_pilot.*positive"),
    "n zero"         = list(function() from_pilot(se_pilot = 2, n_pilot = 0), "n_pilot.*positive"),
    "n NA"           = list(function() from_pilot(se_pilot = 2, n_pilot = NA_real_), "n_pilot.*positive"),
    "interaction NA" = list(function() from_pilot(se_pilot = 2, n_pilot = 50, interaction = NA), "interaction")
  ))
})

# --- Goal functions ---

test_that("find_mde() rejects bad inputs", {
  s <- from_sd(sd_y = 20)
  expect_all_error(list(
    "source not power_source" = list(function() find_mde(list(type = "sd"), n_planned = 500), "power_source"),
    "n_planned negative"      = list(function() find_mde(s, n_planned = -10), "n_planned.*positive"),
    "n_planned zero"          = list(function() find_mde(s, n_planned = 0), "n_planned.*positive"),
    "n_planned NA"            = list(function() find_mde(s, n_planned = NA_real_), "n_planned.*positive"),
    "n_planned fractional"    = list(function() find_mde(s, n = 250.5), "whole number"),
    "ci invalid"              = list(function() find_mde(s, n_planned = 500, ci = 0.80), "ci.*0.90.*0.95"),
    "ci vector"               = list(function() find_mde(s, n = 500, ci = c(0.90, 0.95)), "ci.*0.90 or 0.95")
  ))
  expect_no_error(find_mde(s, n = 250))
})

test_that("find_power() rejects bad inputs", {
  s <- from_sd(sd_y = 20)
  expect_all_error(list(
    "source not power_source" = list(function() find_power("not_a_source", n_planned = 500, tau = 3), "power_source"),
    "n_planned negative"      = list(function() find_power(s, n_planned = -10, tau = 3), "n_planned.*positive"),
    "n_planned NA"            = list(function() find_power(s, n_planned = NA_real_, tau = 3), "n_planned.*positive"),
    "n_planned fractional"    = list(function() find_power(s, n_planned = 250.5, tau = 3), "whole number"),
    "tau zero"                = list(function() find_power(s, n_planned = 500, tau = 0), "tau.*positive"),
    "tau negative"            = list(function() find_power(s, n_planned = 500, tau = -3), "tau.*positive"),
    "tau NA"                  = list(function() find_power(s, n_planned = 500, tau = NA_real_), "tau.*number"),
    "ci character"            = list(function() find_power(s, n = 500, tau = 3, ci = "0.90"), "ci.*0.90 or 0.95")
  ))
})

test_that("find_n() rejects bad inputs", {
  s <- from_sd(sd_y = 20.8)
  expect_all_error(list(
    "source not power_source" = list(function() find_n(42, tau = 3), "power_source"),
    "tau zero"                = list(function() find_n(s, tau = 0), "tau.*positive"),
    "tau negative"            = list(function() find_n(s, tau = -3), "tau.*positive"),
    "tau NA"                  = list(function() find_n(s, tau = NA_real_), "tau.*number"),
    "power NA"                = list(function() find_n(s, tau = 3, power = NA_real_), "power.*between"),
    "power zero"              = list(function() find_n(s, tau = 3, power = 0), "power.*between"),
    "power one"               = list(function() find_n(s, tau = 3, power = 1), "power.*between"),
    "power negative"          = list(function() find_n(s, tau = 3, power = -0.5), "power.*between"),
    "power > 1"               = list(function() find_n(s, tau = 3, power = 1.5), "power.*between"),
    "ci invalid"              = list(function() find_n(s, tau = 3, ci = 0.99), "ci.*0.90.*0.95"),
    "ci NA"                   = list(function() find_n(s, tau = 3, ci = NA), "ci.*0.90 or 0.95"),
    "power below size (90)"   = list(function() find_n(s, tau = 3, power = 0.01), "size of the test \\(0.05\\)"),
    "power at size (90)"      = list(function() find_n(s, tau = 3, power = 0.05), "size of the test \\(0.05\\)"),
    "power at size (95)"      = list(function() find_n(s, tau = 3, power = 0.025, ci = 0.95), "size of the test \\(0.025\\)")
  ))
  # Just above the size of the test is allowed
  expect_no_error(find_n(s, tau = 3, power = 0.04, ci = 0.95))
  expect_no_error(find_n(s, tau = 3, power = 0.06))
})
