# powerrules (development version)

* The package now follows the published version of the paper, "Power Rules: Practical Advice for Computing Statistical Power" (Rules 1-7), and its companion paper, "Automating Power Analysis with Pilot Data." The existing-study and pilot rules are cited as Pilot Rules 1-4 (formerly Rules 7-10), and the MDE factor cites Table 1 (or Appendix Tables 3 and 4 for other power levels).
* `find_mde()`, `find_power()`, and `find_n()` gain a `quiet` argument that computes the result without printing it. `options(powerrules.quiet = TRUE)` sets the default for a session.
* `find_n()` now errors when `power` is at or below the size of the test, where it previously returned a meaningless sample size.
* `find_mde()` and `find_power()` now require a whole-number sample size per condition.
* Power levels such as 97.5% and 99.9% are displayed exactly, and a computed power above 99.5% is reported as ">99%" rather than "100%".
* Large totals no longer print in scientific notation, and inputs are no longer truncated to six decimal places.
* Manuscript sentences report the R-squared of control variables without rounding it to a whole percent.
* `write_loop()` recomputes its expected-power comment when `tau` is overridden.
* `write_manual()` notes in the generated code that it uses the paper's rounded constants.
* `shiny_rules()` explains an out-of-range R-squared, power, or sample size instead of ignoring it.
* The vignettes and bibliography are updated to the published paper.

# powerrules 0.1.0

* Initial release.
