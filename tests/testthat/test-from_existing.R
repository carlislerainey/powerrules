test_that("from_existing() returns a power_source object", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_s3_class(s, "power_source")
})

test_that("from_existing() stores se_existing correctly", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_equal(s$se_existing, 1.8)
})

test_that("from_existing() stores n_existing correctly", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_equal(s$n_existing, 268)
})

test_that("from_existing() stores source type as 'existing'", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_equal(s$type, "existing")
})

test_that("from_existing() stores interaction as FALSE by default", {
  s <- from_existing(se_existing = 1.8, n_existing = 268)
  expect_false(s$interaction)
})

test_that("from_existing() stores interaction = TRUE", {
  s <- from_existing(se_existing = 1.8, n_existing = 268,
                     interaction = TRUE)
  expect_true(s$interaction)
})

test_that("from_existing() errors on non-logical interaction", {
  expect_error(
    from_existing(se_existing = 1.8, n_existing = 268, interaction = "yes"),
    "interaction"
  )
})

test_that("from_existing() errors on NA interaction", {
  expect_error(
    from_existing(se_existing = 1.8, n_existing = 268, interaction = NA),
    "interaction"
  )
})
