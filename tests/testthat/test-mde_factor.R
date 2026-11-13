test_that("mde_factor() gives known values for standard power levels", {
  # 90% CI: qnorm(0.95) + qnorm(power)
  expect_equal(mde_factor(0.80, 0.90), 2.4863, tolerance = 0.001)
  expect_equal(mde_factor(0.95, 0.90), 3.2897, tolerance = 0.001)
  expect_equal(mde_factor(0.90, 0.90), 2.9265, tolerance = 0.001)
  # 95% CI: qnorm(0.975) + qnorm(power)
  expect_equal(mde_factor(0.80, 0.95), 2.8016, tolerance = 0.001)
  expect_equal(mde_factor(0.95, 0.95), 3.6049, tolerance = 0.001)
})

test_that("mde_factor() handles edge cases", {
  # 50% power: qnorm(0.95) + qnorm(0.50) = 1.6449 + 0
  expect_equal(mde_factor(0.50, 0.90), qnorm(0.95), tolerance = 0.001)

  # 99% power
  expect_equal(mde_factor(0.99, 0.90), qnorm(0.95) + qnorm(0.99),
               tolerance = 0.001)
})

test_that("mde_factor() uses correct default ci", {
  expect_equal(mde_factor(0.80), mde_factor(0.80, ci = 0.90))
})
