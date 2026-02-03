test_that("print.power_source() outputs source type", {
  s <- from_sd(sd_y = 20.8)
  out <- capture.output(print(s))
  expect_true(any(grepl("reference population SD", out)))

  s2 <- from_existing(se_existing = 1.8, n_existing = 268)
  out2 <- capture.output(print(s2))
  expect_true(any(grepl("existing study", out2)))

  s3 <- from_pilot(se_pilot = 2.0, n_pilot = 50)
  out3 <- capture.output(print(s3))
  expect_true(any(grepl("pilot data", out3)))
})

test_that("print.power_source() outputs stored values", {
  s <- from_sd(sd_y = 20.8, r_squared = 0.05)
  out <- capture.output(print(s))
  expect_true(any(grepl("20.8", out)))
  expect_true(any(grepl("0.05", out)))
})

test_that("print.power_result() outputs design line", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- capture.output(print(res))
  expect_true(any(grepl("between-subjects", out)))

  res2 <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  out2 <- capture.output(print(res2))
  expect_true(any(grepl("2x2 factorial", out2)))
})

test_that("print.power_result() outputs source line", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- capture.output(print(res))
  expect_true(any(grepl("reference population SD", out)))

  res2 <- from_pilot(se_pilot = 2.0, n_pilot = 50) |>
    find_mde(n_planned = 500)
  out2 <- capture.output(print(res2))
  expect_true(any(grepl("pilot data", out2)))
})

test_that("print.power_result() outputs CI level line", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- capture.output(print(res))
  expect_true(any(grepl("90%", out)))
  expect_true(any(grepl("size-0.05", out)))

  res2 <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500, ci = 0.95)
  out2 <- capture.output(print(res2))
  expect_true(any(grepl("95%", out2)))
  expect_true(any(grepl("size-0.025", out2)))
})

test_that("print.power_result() outputs manuscript sentence", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- capture.output(print(res))
  expect_true(any(grepl("Manuscript sentence", out)))
  expect_false(any(grepl("Rainey", out)))
})

test_that("print.power_result() outputs MDE factor note for find_mde", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- capture.output(print(res))
  expect_true(any(grepl("Note:", out)))
  expect_true(any(grepl("rounds the MDE factor", out)))
})

test_that("print.power_result() outputs MDE factor note for find_n", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3)
  out <- capture.output(print(res))
  expect_true(any(grepl("Note:", out)))
})

test_that("print.power_result() does NOT output MDE factor note for find_power", {
  res <- from_sd(sd_y = 20.8) |> find_power(n_planned = 500, tau = 3)
  out <- capture.output(print(res))
  expect_false(any(grepl("Note:", out)))
})

test_that("print.power_result() says 'treatment effect' when interaction=FALSE", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("treatment effect", out))
})

test_that("print.power_result() says 'interaction' when interaction=TRUE", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("interaction", out))
})

test_that("print.power_result() shows correct total for treatment (2*n)", {
  res <- from_sd(sd_y = 20.8) |> find_mde(n_planned = 500)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("1,000 total", out))
})

test_that("print.power_result() shows correct total for interaction (4*n)", {
  res <- from_sd(sd_y = 2.0, interaction = TRUE) |>
    find_n(tau = 0.67, power = 0.80)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("4 conditions", out))
})

test_that("print.power_result() shows 2x2 factorial for existing + interaction", {
  res <- from_existing(se_existing = 1.8, n_existing = 268,
                       interaction = TRUE) |>
    find_mde(n_planned = 500)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("2x2 factorial", out))
})

test_that("print.power_result() shows 4-condition total for existing + interaction", {
  res <- from_existing(se_existing = 1.8, n_existing = 268,
                       interaction = TRUE) |>
    find_mde(n_planned = 500)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("2,000 total", out))
})

test_that("print.power_result() says 'conservative' for pilot source", {
  res <- from_pilot(se_pilot = 2.13, n_pilot = 85) |>
    find_n(tau = 3, power = 0.95)
  out <- paste(capture.output(print(res)), collapse = " ")
  expect_true(grepl("conservative", out, ignore.case = TRUE))
})

test_that("find_n with ci=0.95 displays qnorm(0.975), not qnorm(0.97)", {
  res <- from_existing(se_existing = 1.8, n_existing = 268) |>
    find_n(tau = 3, ci = 0.95)
  out <- paste(capture.output(print(res)), collapse = "\n")
  expect_true(grepl("qnorm(0.975)", out, fixed = TRUE))
  expect_false(grepl("qnorm(0.97)", out, fixed = TRUE))
})

test_that("from_sd find_n sentence has 'respondents per condition' once", {
  res <- from_sd(sd_y = 20.8) |> find_n(tau = 3, power = 0.95)
  out <- capture.output(print(res))
  sentence_lines <- out[grep("Manuscript sentence", out):length(out)]
  sentence <- paste(sentence_lines, collapse = " ")
  matches <- gregexpr("respondents per condition", sentence)[[1]]
  expect_equal(sum(matches > 0), 1)
})
