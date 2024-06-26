#' Check Inputs
#'
#' @description
#' Function to check that the inputs given to the function \code{opt_des} are correct. If not, throws the
#' correspondent error message.
#'
#' @inheritParams opt_des
#'
#'
check_inputs <- function(criterion, model, parameters, par_values, design_space,
                         init_design,
                         join_thresh,
                         delete_thresh,
                         delta,
                         tol,
                         tol2,
                         par_int,
                         matB,
                         reg_int,
                         desired_output,
                         weight_fun) {
  error_msg <- ""
  # Check for valid criterion
  if (!criterion %in% c("D-Optimality", "Ds-Optimality", "A-Optimality", "I-Optimality", "L-Optimality")) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " Choose a valid criterion: D-Optimality, Ds-Optimality, A-Optimality, I-Optimality or L-Optimality")
  }

  # Check that the model is a formula
  if (!rlang::is_formula(model)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " The model must be a formula")
  }

  # Check that parameters is a character vector
  if (!is.character(parameters) || !is.atomic(parameters) || !is.vector(parameters)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " parameters must be a character vector")
  }

  # check that all the parameters are in the formula
  if (!all(purrr::map_lgl(parameters, function(x) grepl(x, paste(format(model),collapse=""))))) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " The model must contain all the parameters")
  }
  # Check that x is in the formula
  if (!grepl("x", paste(format(model),collapse=""))) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " The model must use x as the variable")
  }

  # Check that par_values is a numeric vector
  if (!is.numeric(par_values) || !is.atomic(par_values) || !is.vector(par_values)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " par_values must be a numeric vector")
  }

  # Check that there's a nominal value for each parameter
  if (length(parameters) != length(par_values)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " parameters must be of the same length as par_values")
  }

  # Check that design space is a length 2 numeric vector
  if (!is.numeric(design_space) || !is.atomic(design_space) || !is.vector(design_space) || length(design_space) != 2) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " design_space must be numeric vector of length 2")
  }

  # Check that init_design is a dataframe
  if (!is.data.frame(init_design)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " init_design must be a dataframe")
  }

  # Check the names of init_design
  if (!identical(names(init_design), c("Point", "Weight"))) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " init_design must have two columns: Point and Weight")
  }

  # Check that join_thresh is numeric
  if (!is.numeric(join_thresh)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " join_thresh must be numeric")
  }

  # Check that delete_thresh is numeric and between 0 and 1
  if (!is.numeric(delete_thresh)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " delete_thresh must be numeric")
  }

  if (delete_thresh >= 1 || delete_thresh <= 0) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " delete_thresh must be in (0, 1)")
  }


  # Check that delta is numeric and between 0 and 1
  if (!is.numeric(delta)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " delta must be numeric")
  }

  if (delta >= 1 || delta <= 0) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " delta must be in (0, 1)")
  }

  # Check that tol is numeric and greater than 0
  if (!is.numeric(tol)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " tol must be numeric")
  }

  if (tol <= 0) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " tol must be in greater than 0")
  }

  # Check that tol2 is numeric and greater than 0
  if (!is.numeric(tol2)) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " tol2 must be numeric")
  }

  if (tol2 <= 0) {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " tol2 must be in greater than 0")
  }


  # If Ds-Opt check par int
  if (criterion == "Ds-Optimality") {
    # Check that par_int is a numeric vector
    if (!is.numeric(par_int) || !is.atomic(par_int) || !is.vector(par_int)) {
      error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " par_int must be numeric vector")
    }
    # Check that all are indexes
    if (!is.null(par_int)) {
      if (any(par_int %% 1 != 0)) {
        error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " par_int must contain only integers")
      }
      if (any(par_int > length(parameters))) {
        error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " par_int index out of bonds")
      }
      if (length(par_int) >= length(parameters)) {
        error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " par_int has length equal or greater than the number of parameters. Consider using D-Optimality")
      }
    }
  }

  # If I-Optimality check reg_int
  if (criterion == "I-Optimality") {
    # Check that matB or reg_int are provided
    if (is.null(reg_int)) {
      error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " reg_int must be provided for I-Optimality")
    }
    # Check that reg_int is a numeric vector of length 2
    if (!is.null(reg_int) && (!is.numeric(reg_int) || !is.atomic(reg_int) || !is.vector(reg_int) || length(reg_int) != 2)) {
      error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " reg_int must be numeric vector of length 2")
    }
  }
  # If L-Optimality check matB
  if (criterion == "L-Optimality") {
    # Check that matB is provided
    if (is.null(matB)) {
      error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " matB must be provided for L-Optimality")
    }
    # Check that matB is a matrix with the correct length
    if (!is.null(matB) && (!is.matrix(matB) || nrow(matB) != length(parameters) || ncol(matB) != length(parameters))) {
      error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " matB must be a square matrix with as many rows as parameters")
    }
  }
  if(!is.function(weight_fun)){
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " weight_fun must be a function")
  }

  # If there are errors, send them to the user
  if (!identical(error_msg, "")) {
    stop(error_msg, call. = FALSE)
  }
}
