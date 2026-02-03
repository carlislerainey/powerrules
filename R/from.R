#' Create a power source from a reference population SD
#'
#' Uses Rules 3 and 4 from Rainey (2026). The SD of the outcome in a
#' reference population is used to predict the SE of the planned study.
#'
#' @param sd_y Standard deviation of the outcome in a reference population.
#' @param r_squared R-squared of control variables (default 0, no adjustment).
#' @param interaction Logical; TRUE for a 2x2 factorial interaction.
#' @return An object of class `"power_source"`.
#' @export
#' @examples
#' from_sd(sd_y = 20.8) |> find_mde(n = 500)
from_sd <- function(sd_y, r_squared = 0, interaction = FALSE) {
  if (!is.numeric(sd_y) || length(sd_y) != 1 || is.na(sd_y) || sd_y <= 0)
    stop("`sd_y` must be a positive number")
  if (!is.numeric(r_squared) || length(r_squared) != 1 || is.na(r_squared) ||
      r_squared < 0 || r_squared >= 1)
    stop("`r_squared` must be between 0 (inclusive) and 1 (exclusive)")
  if (!is.logical(interaction) || length(interaction) != 1 || is.na(interaction))
    stop("`interaction` must be TRUE or FALSE")

  structure(list(
    type = "sd",
    sd_y = sd_y,
    r_squared = r_squared,
    interaction = interaction
  ), class = "power_source")
}

#' Create a power source from an existing study
#'
#' Uses Rule 7 from Rainey (2026). The SE and sample size from an existing
#' study are used to predict the SE of the planned study.
#'
#' @param se_existing Standard error from the existing study.
#' @param n_existing Respondents per condition in the existing study.
#' @param interaction Logical; TRUE for a 2x2 factorial interaction. The SE
#'   computation is unchanged (the existing SE already encodes the design),
#'   but totals, design labels, and manuscript sentences will reflect the
#'   factorial structure.
#' @return An object of class `"power_source"`.
#' @export
#' @examples
#' from_existing(se_existing = 1.8, n_existing = 268) |>
#'   find_mde(n_planned = 500)
from_existing <- function(se_existing, n_existing, interaction = FALSE) {
  if (!is.numeric(se_existing) || length(se_existing) != 1 || is.na(se_existing) || se_existing <= 0)
    stop("`se_existing` must be a positive number")
  if (!is.numeric(n_existing) || length(n_existing) != 1 || is.na(n_existing) || n_existing <= 0)
    stop("`n_existing` must be a positive number")
  if (!is.logical(interaction) || length(interaction) != 1 || is.na(interaction))
    stop("`interaction` must be TRUE or FALSE")

  structure(list(
    type = "existing",
    se_existing = se_existing,
    n_existing = n_existing,
    interaction = interaction
  ), class = "power_source")
}

#' Create a power source from pilot data
#'
#' Uses Rule 9 from Rainey (2026). The SE and sample size from a pilot study
#' are used to predict the SE of the planned study, with a conservative
#' adjustment for pilot noise.
#'
#' @param se_pilot Standard error from the pilot study.
#' @param n_pilot Respondents per condition in the pilot study.
#' @param interaction Logical; TRUE for a 2x2 factorial interaction (changes
#'   the conservative adjustment factor).
#' @return An object of class `"power_source"`.
#' @export
#' @examples
#' from_pilot(se_pilot = 2.13, n_pilot = 85) |>
#'   find_mde(n_planned = 500)
from_pilot <- function(se_pilot, n_pilot, interaction = FALSE) {
  if (!is.numeric(se_pilot) || length(se_pilot) != 1 || is.na(se_pilot) || se_pilot <= 0)
    stop("`se_pilot` must be a positive number")
  if (!is.numeric(n_pilot) || length(n_pilot) != 1 || is.na(n_pilot) || n_pilot <= 0)
    stop("`n_pilot` must be a positive number")
  if (!is.logical(interaction) || length(interaction) != 1 || is.na(interaction))
    stop("`interaction` must be TRUE or FALSE")

  structure(list(
    type = "pilot",
    se_pilot = se_pilot,
    n_pilot = n_pilot,
    interaction = interaction
  ), class = "power_source")
}
