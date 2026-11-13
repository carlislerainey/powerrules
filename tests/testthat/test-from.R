# --- from_sd() ---

test_that("from_sd() stores its class, type, and every input", {
  s <- from_sd(sd_y = 20.8)
  expect_s3_class(s, "power_source")
  expect_equal(s$type, "sd")
  expect_equal(s$sd_y, 20.8)
  expect_equal(s$r_squared, 0)
  expect_false(s$interaction)

  s2 <- from_sd(sd_y = 2.0, r_squared = 0.05, interaction = TRUE)
  expect_equal(s2$sd_y, 2.0)
  expect_equal(s2$r_squared, 0.05)
  expect_true(s2$interaction)
})

# --- from_existing() ---

test_that("from_existing() stores its class, type, and every input", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_s3_class(s, "power_source")
  expect_equal(s$type, "existing")
  expect_equal(s$se_existing, 1.8)
  expect_equal(s$n_existing, 268)
  expect_false(s$interaction)

  s2 <- from_existing(se_existing = 1.8, n_existing = 268, interaction = TRUE)
  expect_true(s2$interaction)
})

# --- from_pilot() ---

test_that("from_pilot() stores its class, type, and every input", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_s3_class(s, "power_source")
  expect_equal(s$type, "pilot")
  expect_equal(s$se_pilot, 2.0)
  expect_equal(s$n_pilot, 50)
  expect_false(s$interaction)

  s2 <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE)
  expect_true(s2$interaction)
})
