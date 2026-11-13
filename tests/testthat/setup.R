# The goal functions print their full output by default. Silence them for the
# test run; tests that check the printed output call print() on the result
# or pass quiet = FALSE. The option is restored when the run ends.
old_options <- options(powerrules.quiet = TRUE)
withr::defer(options(old_options), teardown_env())
