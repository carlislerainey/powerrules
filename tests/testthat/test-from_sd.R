test_that("from_sd() returns a power_source object", {
  s <- from_sd(sd_y = 20.8)
  expect_s3_class(s, "power_source")
})

test_that("from_sd() stores sd_y correctly", {
  s <- from_sd(sd_y = 20.8)
  expect_equal(s$sd_y, 20.8)
})

test_that("from_sd() stores r_squared correctly (default 0)", {
  s <- from_sd(sd_y = 20.8)
  expect_equal(s$r_squared, 0)
})

test_that("from_sd() stores r_squared correctly (non-zero)", {
  s <- from_sd(sd_y = 20.8, r_squared = 0.05)
  expect_equal(s$r_squared, 0.05)
})

test_that("from_sd() stores interaction correctly (default FALSE)", {
  s <- from_sd(sd_y = 20.8)
  expect_false(s$interaction)
})

test_that("from_sd() stores interaction correctly (TRUE)", {
  s <- from_sd(sd_y = 2.0, interaction = TRUE)
  expect_true(s$interaction)
})

test_that("from_sd() stores source type as 'sd'", {
  s <- from_sd(sd_y = 20.8)
  expect_equal(s$type, "sd")
})
