# Packages required by each Shiny app beyond those already in Imports.
# Checked simultaneously so the user learns about all missing ones at once.
.shiny_needed <- function(app) {
  common <- c("shinydashboard", "shinyalert", "shinyjs", "plotly",
              "DT", "hrbrthemes", "markdown", "magrittr", "tidyverse")
  if (app == "augment") c(common, "orthopolynom") else common
}

.shiny_check <- function(app_name, url) {
  if (!interactive()) {
    message(app_name, "() requires an interactive R session. ",
            "Access the app online: ", url)
    return(invisible(FALSE))
  }
  missing_pkgs <- .shiny_needed(app_name)
  missing_pkgs <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are needed but not installed: ",
      paste(missing_pkgs, collapse = ", "), ".\n",
      "Install with:\n  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))\n",
      "Or access the app online: ", url,
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Shiny Optimal
#'
#' @description
#' Launches the demo Shiny application to calculate optimal designs for
#' Antoine's Equation. Requires an interactive R session; if called
#' non-interactively a message with the web URL is emitted instead.
#'
#' @export
#'
#' @examples
#' shiny_optimal()
shiny_optimal <- function() {
  url <- "https://kezrael.shinyapps.io/AntoineOptimal/"
  if (!isTRUE(.shiny_check("optimal", url))) return(invisible(NULL))
  appDir <- system.file("shiny-examples", "D-Optimality", package = "optedr")
  if (appDir == "")
    stop("Could not find the app directory. Try re-installing optedr.",
         call. = FALSE)
  shiny::runApp(appDir, display.mode = "normal")
}


#' Shiny D-augment
#'
#' @description
#' Launches the demo Shiny application to D-augment several pre-specified
#' models. Requires an interactive R session; if called non-interactively
#' a message with the web URL is emitted instead.
#'
#' @export
#'
#' @examples
#' shiny_augment()
shiny_augment <- function() {
  url <- "https://kezrael.shinyapps.io/AddPoints/"
  if (!isTRUE(.shiny_check("augment", url))) return(invisible(NULL))
  appDir <- system.file("shiny-examples", "AddPoints", package = "optedr")
  if (appDir == "")
    stop("Could not find the app directory. Try re-installing optedr.",
         call. = FALSE)
  shiny::runApp(appDir, display.mode = "normal")
}
