#' Shiny Optimal
#'
#' @return
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
#' @return
#' @export
#'
shinyAugment <- function() {
  appDir <- system.file("shiny-examples", "AddPoints", package = "optedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
