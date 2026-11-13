# Helpers for testing the power_rules() interactive guide. The ask_* wrappers
# and check_interactive() are mocked so the whole orchestration can run
# without a console.

# Mock that returns `responses` in sequence (later calls past the end error).
make_menu_mock <- function(responses) {
  idx <- 0L
  function(prompt, choices) {
    idx <<- idx + 1L
    responses[[idx]]
  }
}

make_number_mock <- function(responses) {
  idx <- 0L
  function(prompt, hint = NULL) {
    idx <<- idx + 1L
    responses[[idx]]
  }
}

# Run power_rules() with scripted answers.
#   menu      menu letters, in the order they are asked
#   numbers   numeric answers, in the order they are asked
#   r_squared, power  one value, or several to be returned in sequence (the
#             guide re-asks when an answer is unusable)
# Returns list(res = the power_result, out = printed lines,
#              n_power_asked = number of ask_power() calls).
run_guide <- function(menu, numbers, r_squared = 0, power = 0.80) {
  n_power_asked <- 0L
  r2_idx <- 0L
  local_mocked_bindings(
    check_interactive = function() invisible(NULL),
    ask_menu = make_menu_mock(as.list(menu)),
    ask_number = make_number_mock(as.list(numbers)),
    ask_r_squared = function(prompt, hint = NULL) {
      r2_idx <<- r2_idx + 1L
      r_squared[min(r2_idx, length(r_squared))]
    },
    ask_power = function(prompt, hint = NULL) {
      n_power_asked <<- n_power_asked + 1L
      power[min(n_power_asked, length(power))]
    },
    ask_divider = function() invisible(NULL),
    .package = "powerrules"
  )
  out <- capture.output(res <- power_rules())
  list(res = res, out = out, n_power_asked = n_power_asked)
}
