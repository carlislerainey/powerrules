# Behavior shared by write_manual(), write_loop(), and write_DeclareDesign()

writers <- list(
  write_manual = write_manual,
  write_loop = write_loop,
  write_DeclareDesign = write_DeclareDesign
)
sim_writers <- writers[c("write_loop", "write_DeclareDesign")]

# How each simulation writer spells the effect size in its code
tau_code <- function(name, tau) {
  switch(name,
    write_loop = paste0("tau  <- ", tau),
    write_DeclareDesign = paste0(tau, " * Z"))
}

# --- Input validation --------------------------------------------------------

test_that("every writer rejects a non-power_result", {
  for (w in writers)
    expect_error(w("not a result"), "must be a power_result object")
})

test_that("every writer rejects interaction designs", {
  res <- from_sd(sd_y = 10, interaction = TRUE) |>
    find_power(n_planned = 100, tau = 2)
  for (w in writers)
    expect_error(w(res), "does not yet support interaction designs")
})

test_that("simulation writers reject an invalid tau", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 100, tau = 2)
  for (w in sim_writers) {
    expect_error(w(res, tau = -1), "must be a positive number")
    expect_error(w(res, tau = NA), "must be a positive number")
    expect_error(w(res, tau = "big"), "must be a positive number")
    expect_error(w(res, tau = c(1, 2)), "must be a positive number")
  }
})

# --- Return value ------------------------------------------------------------

test_that("every writer returns the code as a single string, invisibly", {
  res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  for (w in writers) {
    out <- capture.output(v <- withVisible(w(res)))
    expect_false(v$visible)
    expect_type(v$value, "character")
    expect_length(v$value, 1)
  }
})

# --- Choice of tau -----------------------------------------------------------

test_that("simulation writers use the result's tau for find_power and find_n", {
  power_res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  n_res <- from_sd(sd_y = 10) |> find_n(tau = 4)
  for (nm in names(sim_writers)) {
    w <- sim_writers[[nm]]
    expect_match(run_writer(w, power_res)$code, tau_code(nm, 3), fixed = TRUE)
    expect_match(run_writer(w, n_res)$code, tau_code(nm, 4), fixed = TRUE)
  }
})

test_that("simulation writers default to the MDE at 80% power for find_mde", {
  res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200)
  mde_str <- formatC(res$mde_80, format = "f", digits = 2)
  for (w in sim_writers) {
    got <- run_writer(w, res)
    expect_match(got$code, mde_str, fixed = TRUE)
    expect_true(any(grepl("MDE at 80% power", got$out)))
  }
})

test_that("an explicit tau overrides the result's tau", {
  mde_res <- from_sd(sd_y = 10) |> find_mde(n_planned = 200)
  power_res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  for (nm in names(sim_writers)) {
    w <- sim_writers[[nm]]
    got <- run_writer(w, mde_res, tau = 5)
    expect_match(got$code, tau_code(nm, 5), fixed = TRUE)
    expect_false(any(grepl("MDE at 80% power", got$out)))
    expect_match(run_writer(w, power_res, tau = 7)$code,
                 tau_code(nm, 7), fixed = TRUE)
  }
})

# --- Back-calculated SD note -------------------------------------------------

test_that("simulation writers note a back-calculated SD only without from_sd()", {
  sd_res <- from_sd(sd_y = 10) |> find_power(n_planned = 200, tau = 3)
  existing_res <- from_existing(se_existing = 2, n_existing = 100) |>
    find_power(n_planned = 200, tau = 3)
  pilot_res <- from_pilot(se_pilot = 2, n_pilot = 50) |>
    find_power(n_planned = 200, tau = 3)
  for (w in sim_writers) {
    expect_false(any(grepl("back-calculated", run_writer(w, sd_res)$out)))
    expect_true(any(grepl("back-calculated SD", run_writer(w, existing_res)$out)))
    expect_true(any(grepl("back-calculated SD", run_writer(w, pilot_res)$out)))
  }
})
