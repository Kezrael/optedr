#' Shiny Optimal
#'
#' @description
#' Launches the demo shiny application to calculate optimal designs for Antoine's Equation
#'
#' @export
#'
shinyOptimal <- function() {
  appDir <- system.file("shiny-examples", "D-Optimality", package = "optedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}



#' Shiny D-augment
#'
#' @description
#' Launches the demo shiny application to D-augment several preespecified models
#'
#' @export
#'
shinyAugment <- function() {
  appDir <- system.file("shiny-examples", "AddPoints", package = "optedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
