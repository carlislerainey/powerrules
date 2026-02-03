test_that("from_pilot() returns a power_source object", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_s3_class(s, "power_source")
})

test_that("from_pilot() stores se_pilot correctly", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_equal(s$se_pilot, 2.0)
})

test_that("from_pilot() stores n_pilot correctly", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_equal(s$n_pilot, 50)
})

test_that("from_pilot() stores interaction correctly (default FALSE)", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_false(s$interaction)
})

test_that("from_pilot() stores interaction correctly (TRUE)", {
  s <- from_pilot(se_pilot = 0.40, n_pilot = 75, interaction = TRUE)
  expect_true(s$interaction)
})

test_that("from_pilot() stores source type as 'pilot'", {
  s <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  expect_equal(s$type, "pilot")
})
