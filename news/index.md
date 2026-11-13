# Changelog

## powerrules (development version)

- The package now follows the published version of the paper, “Power
  Rules: Practical Advice for Computing Statistical Power” (Rules 1-7),
  and its companion paper, “Automating Power Analysis with Pilot Data.”
  The existing-study and pilot rules are cited as Pilot Rules 1-4
  (formerly Rules 7-10), and the MDE factor cites Table 1 (or Appendix
  Tables 3 and 4 for other power levels).
- [`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md),
  [`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md),
  and
  [`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md)
  gain a `quiet` argument that computes the result without printing it.
  `options(powerrules.quiet = TRUE)` sets the default for a session.
- [`find_n()`](https://carlislerainey.github.io/powerrules/reference/find_n.md)
  now errors when `power` is at or below the size of the test, where it
  previously returned a meaningless sample size.
- [`find_mde()`](https://carlislerainey.github.io/powerrules/reference/find_mde.md)
  and
  [`find_power()`](https://carlislerainey.github.io/powerrules/reference/find_power.md)
  now require a whole-number sample size per condition.
- Power levels such as 97.5% and 99.9% are displayed exactly, and a
  computed power above 99.5% is reported as “\>99%” rather than “100%”.
- Large totals no longer print in scientific notation, and inputs are no
  longer truncated to six decimal places.
- Manuscript sentences report the R-squared of control variables without
  rounding it to a whole percent.
- [`write_loop()`](https://carlislerainey.github.io/powerrules/reference/write_loop.md)
  recomputes its expected-power comment when `tau` is overridden.
- [`write_manual()`](https://carlislerainey.github.io/powerrules/reference/write_manual.md)
  notes in the generated code that it uses the paper’s rounded
  constants.
- [`shiny_rules()`](https://carlislerainey.github.io/powerrules/reference/shiny_rules.md)
  explains an out-of-range R-squared, power, or sample size instead of
  ignoring it.
- The vignettes and bibliography are updated to the published paper.

## powerrules 0.1.0

- Initial release.
