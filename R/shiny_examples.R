#' Shiny Optimal
#'
#' @description
#' Launches the demo shiny application to calculate optimal designs for Antoine's Equation
#'
#' @export
#'
shiny_optimal <- function() {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("Package \"tidyverse\" needed for this function to work. Please install it or access through https://kezrael.shinyapps.io/AntoineOptimal/.",
         call. = FALSE)
  } else if(!requireNamespace("markdown", quietly = TRUE)) {
    stop("Package \"markdown\" needed for this function to work. Please install it or access through https://kezrael.shinyapps.io/AntoineOptimal/.",
         call. = FALSE)
  }
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
shiny_augment <- function() {
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("Package \"tidyverse\" needed for this function to work. Please install it or access through https://kezrael.shinyapps.io/AddPoints/.",
         call. = FALSE)
  } else if(!requireNamespace("markdown", quietly = TRUE)) {
    stop("Package \"markdown\" needed for this function to work. Please install it or access through https://kezrael.shinyapps.io/AddPoints/.",
         call. = FALSE)
  }
  appDir <- system.file("shiny-examples", "AddPoints", package = "optedr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optedr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
