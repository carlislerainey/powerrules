# --- from_sd() validation ---

test_that("from_sd() errors on negative sd_y", {
  expect_error(from_sd(sd_y = -1), "sd_y.*positive")
})

test_that("from_sd() errors on sd_y = 0", {
  expect_error(from_sd(sd_y = 0), "sd_y.*positive")
})

test_that("from_sd() errors on r_squared < 0", {
  expect_error(from_sd(sd_y = 20, r_squared = -0.1), "r_squared")
})

test_that("from_sd() errors on r_squared >= 1", {
  expect_error(from_sd(sd_y = 20, r_squared = 1), "r_squared")
  expect_error(from_sd(sd_y = 20, r_squared = 1.5), "r_squared")
})

test_that("from_sd() errors on non-logical interaction", {
  expect_error(from_sd(sd_y = 20, interaction = "yes"), "interaction")
})

# --- NA input handling ---

test_that("from_sd() errors on NA inputs", {
  expect_error(from_sd(sd_y = NA_real_), "sd_y.*positive")
  expect_error(from_sd(sd_y = 20, r_squared = NA_real_), "r_squared")
  expect_error(from_sd(sd_y = 20, interaction = NA), "interaction")
})

test_that("from_existing() errors on NA inputs", {
  expect_error(from_existing(se_existing = NA_real_, n_existing = 100),
               "se_existing.*positive")
  expect_error(from_existing(se_existing = 1.8, n_existing = NA_real_),
               "n_existing.*positive")
})

test_that("from_pilot() errors on NA inputs", {
  expect_error(from_pilot(se_pilot = NA_real_, n_pilot = 50),
               "se_pilot.*positive")
  expect_error(from_pilot(se_pilot = 2, n_pilot = NA_real_),
               "n_pilot.*positive")
  expect_error(from_pilot(se_pilot = 2, n_pilot = 50, interaction = NA),
               "interaction")
})

test_that("find_mde() errors on NA n_planned", {
  s <- from_sd(sd_y = 20)
  expect_error(find_mde(s, n_planned = NA_real_), "n_planned.*positive")
})

test_that("find_power() errors on NA inputs", {
  s <- from_sd(sd_y = 20)
  expect_error(find_power(s, n_planned = NA_real_, tau = 3),
               "n_planned.*positive")
  expect_error(find_power(s, n_planned = 500, tau = NA_real_),
               "tau.*number")
})

test_that("find_n() errors on NA inputs", {
  s <- from_sd(sd_y = 20)
  expect_error(find_n(s, tau = NA_real_), "tau.*number")
  expect_error(find_n(s, tau = 3, power = NA_real_), "power.*between")
})

# --- from_existing() validation ---

test_that("from_existing() errors on negative se_existing", {
  expect_error(from_existing(se_existing = -1, n_existing = 100),
               "se_existing.*positive")
})

test_that("from_existing() errors on se_existing = 0", {
  expect_error(from_existing(se_existing = 0, n_existing = 100),
               "se_existing.*positive")
})

test_that("from_existing() errors on negative n_existing", {
  expect_error(from_existing(se_existing = 1.8, n_existing = -10),
               "n_existing.*positive")
})

test_that("from_existing() errors on n_existing = 0", {
  expect_error(from_existing(se_existing = 1.8, n_existing = 0),
               "n_existing.*positive")
})

# --- from_pilot() validation ---

test_that("from_pilot() errors on negative se_pilot", {
  expect_error(from_pilot(se_pilot = -1, n_pilot = 50), "se_pilot.*positive")
})

test_that("from_pilot() errors on se_pilot = 0", {
  expect_error(from_pilot(se_pilot = 0, n_pilot = 50), "se_pilot.*positive")
})

test_that("from_pilot() errors on negative n_pilot", {
  expect_error(from_pilot(se_pilot = 2, n_pilot = -5), "n_pilot.*positive")
})

test_that("from_pilot() errors on n_pilot = 0", {
  expect_error(from_pilot(se_pilot = 2, n_pilot = 0), "n_pilot.*positive")
})

# --- find_mde() validation ---

test_that("find_mde() errors if source is not a power_source", {
  expect_error(find_mde(list(type = "sd"), n_planned = 500), "power_source")
})

test_that("find_mde() errors on negative n_planned", {
  s <- from_sd(sd_y = 20)
  expect_error(find_mde(s, n_planned = -10), "n_planned.*positive")
})

test_that("find_mde() errors on n_planned = 0", {
  s <- from_sd(sd_y = 20)
  expect_error(find_mde(s, n_planned = 0), "n_planned.*positive")
})

test_that("find_mde() errors on invalid ci", {
  s <- from_sd(sd_y = 20)
  expect_error(find_mde(s, n_planned = 500, ci = 0.80), "ci.*0.90.*0.95")
})

# --- find_power() validation ---

test_that("find_power() errors if source is not a power_source", {
  expect_error(find_power("not_a_source", n_planned = 500, tau = 3),
               "power_source")
})

test_that("find_power() errors on negative n_planned", {
  s <- from_sd(sd_y = 20)
  expect_error(find_power(s, n_planned = -10, tau = 3), "n_planned.*positive")
})

test_that("find_power() errors on tau = 0", {
  s <- from_sd(sd_y = 20)
  expect_error(find_power(s, n_planned = 500, tau = 0), "tau.*positive")
})

test_that("find_power() errors on negative tau with informative message", {
  s <- from_sd(sd_y = 20)
  expect_error(find_power(s, n_planned = 500, tau = -3), "tau.*positive")
})

# --- find_n() validation ---

test_that("find_n() errors if source is not a power_source", {
  expect_error(find_n(42, tau = 3), "power_source")
})

test_that("find_n() errors on tau = 0", {
  s <- from_sd(sd_y = 20)
  expect_error(find_n(s, tau = 0), "tau.*positive")
})

test_that("find_n() errors on negative tau with informative message", {
  s <- from_sd(sd_y = 20)
  expect_error(find_n(s, tau = -3), "tau.*positive")
})

test_that("find_n() errors on power <= 0 or power >= 1", {
  s <- from_sd(sd_y = 20)
  expect_error(find_n(s, tau = 3, power = 0), "power.*between")
  expect_error(find_n(s, tau = 3, power = 1), "power.*between")
  expect_error(find_n(s, tau = 3, power = -0.5), "power.*between")
  expect_error(find_n(s, tau = 3, power = 1.5), "power.*between")
})

test_that("find_n() errors on invalid ci", {
  s <- from_sd(sd_y = 20)
  expect_error(find_n(s, tau = 3, ci = 0.99), "ci.*0.90.*0.95")
})
