# --- Shared helpers for the write_*() code generators (internal) -------------

# Validate an optional `tau` override and settle the effect size and outcome
# SD that the two simulation generators use. Without an override, `tau` is
# the result's own (find_power, find_n) or the MDE at 80% power (find_mde).
# A source other than from_sd() has no SD, so one is back-calculated from the
# predicted SE.
sim_setup <- function(result, tau = NULL) {
  if (!is.null(tau)) check_positive(tau, "tau")

  source <- result$source
  tau_from_mde <- is.null(tau) && result$goal == "mde"
  if (is.null(tau)) tau <- if (tau_from_mde) result$mde_80 else result$tau

  sd_backcalc <- source$type != "sd"
  sd <- if (sd_backcalc) result$se * sqrt(result$n_planned / 2)
        else source$sd_y * sqrt(1 - source$r_squared)
  sd_as_typed <- !sd_backcalc && source$r_squared == 0

  list(
    tau = tau,
    tau_from_mde = tau_from_mde,
    tau_str = if (tau_from_mde) fmt(tau) else fmt_val(tau),
    sd_backcalc = sd_backcalc,
    sd_str = if (sd_as_typed) fmt_val(sd) else fmt(sd)
  )
}

# Print the notes that explain a defaulted tau or a back-calculated SD.
# `fn` is the calling function's name; `what` names the generated code.
print_sim_notes <- function(sim, fn, what) {
  if (sim$tau_from_mde)
    print_note("Note: Using tau = ", fmt(sim$tau), " (MDE at 80% power). ",
               "Override with ", fn, "(result, tau = ...).")
  if (sim$sd_backcalc)
    print_note("Note: The ", what, " uses a back-calculated SD ",
               "(from the predicted SE). For a more natural mapping, use ",
               "from_sd() as the source.")
}

# Print a wrapped paragraph after a blank line
print_note <- function(...) {
  cat("\n")
  cat(wrap_sentence(paste0(...)), "\n")
}

# Print generated code after a blank line, indented two spaces
print_code <- function(code) {
  cat("\n")
  cat(paste0("  ", strsplit(code, "\n")[[1]], "\n"), sep = "")
}
