# The quiet argument and the powerrules.quiet option. setup.R turns the
# option on for the whole test run, so these tests set it explicitly.

test_that("goal functions print by default", {
  withr::local_options(powerrules.quiet = NULL)
  s <- from_sd(sd_y = 20.8)
  expect_output(find_mde(s, n = 500), "Power Analysis")
  expect_output(find_power(s, n = 500, tau = 3), "Power Analysis")
  expect_output(find_n(s, tau = 3), "Power Analysis")
})

test_that("quiet = TRUE computes the same result without printing", {
  withr::local_options(powerrules.quiet = NULL)
  s <- from_sd(sd_y = 20.8)
  expect_silent(res <- find_mde(s, n = 500, quiet = TRUE))
  expect_s3_class(res, "power_result")
  expect_equal(res$mde_80, mde_factor(0.80) * res$se)
  expect_silent(find_power(s, n = 500, tau = 3, quiet = TRUE))
  expect_silent(find_n(s, tau = 3, quiet = TRUE))

  # The result still prints on request
  expect_output(print(res), "Manuscript sentence")
})

test_that("the powerrules.quiet option sets the default", {
  s <- from_sd(sd_y = 20.8)
  withr::local_options(powerrules.quiet = TRUE)
  expect_silent(find_mde(s, n = 500))
  expect_silent(find_power(s, n = 500, tau = 3))
  expect_silent(find_n(s, tau = 3))

  # The argument overrides the option
  expect_output(find_mde(s, n = 500, quiet = FALSE), "Power Analysis")
})

test_that("quiet must be TRUE or FALSE", {
  s <- from_sd(sd_y = 20.8)
  expect_error(find_mde(s, n = 500, quiet = "yes"), "quiet.*TRUE or FALSE")
  expect_error(find_power(s, n = 500, tau = 3, quiet = NA),
               "quiet.*TRUE or FALSE")
  expect_error(find_n(s, tau = 3, quiet = c(TRUE, FALSE)),
               "quiet.*TRUE or FALSE")
})

test_that("power_rules() shows the calculation even when the option is on", {
  withr::local_options(powerrules.quiet = TRUE)
  g <- run_guide(menu = c("a", "a", "a", "a", "a"), numbers = c(20.8, 500))
  expect_true(any(grepl("Predicted SE", g$out, fixed = TRUE)))
  expect_true(any(grepl("Manuscript sentence", g$out, fixed = TRUE)))
})
