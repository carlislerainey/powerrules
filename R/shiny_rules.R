#' Browser-based power analysis guide
#'
#' Opens a Shiny app that walks through a power analysis on a virtual
#' whiteboard. The app collects inputs, shows step-by-step formulas with
#' color-coded values, and produces a manuscript-ready sentence and a
#' copyable pipeline.
#'
#' @return Called for its side effect (launches the app). Returns the
#'   value of `shiny::runApp()` invisibly.
#' @export
shiny_rules <- function() {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("shiny_rules() requires the shiny package. ",
         "Install it with install.packages(\"shiny\")")
  shiny::runApp(system.file("shiny", package = "powerrules"))
}
