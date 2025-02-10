# Optimality Criteria -----------------------

#' Master function for the criterion function
#'
#' @description
#' Depending on the criterion input, the function returns the output of the corresponding criterion function given
#' the information matrix.
#'
#' @param criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#'   * 'L-Optimality'
#' @param M information matrix for which the criterion value wants to be calculated.
#' @param k numeric variable with the number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#' @param par_int numeric vector with the index of the parameters of interest of the model. Only for "Ds-Optimality".
#' @param matB optional matrix of dimensions k x k, for I- and L-optimality.
#'
#' @return Numeric value of the optimality criterion for the information matrix.
crit <- function(criterion, M, k = 0, par_int = c(1), matB = NA) {
  if (identical(criterion, "D-Optimality")) {
    return(dcrit(M, k))
  }
  else if (identical(criterion, "Ds-Optimality")) {
    return(dscrit(M, par_int))
  }
  else if (identical(criterion, "A-Optimality")) {
    return(icrit(M, diag(k)))
  }
  else if (identical(criterion, "I-Optimality") || identical(criterion, "L-Optimality")) {
    return(icrit(M, matB))
  }
}


#' Criterion function for D-Optimality
#'
#' @description
#' Calculates the value of the D-Optimality criterion function, which follows the expression:
#' \deqn{\phi_D = \left(\frac{1}{|M|}\right)^{1/k}}
#'
#'
#' @param M information matrix for which the criterion value wants to be calculated.
#' @param k numeric variable with the number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#'
#'
#' @return numeric value of the D-optimality criterion for the information matrix.
dcrit <- function(M, k) {
  if (k == 0) k <- nrow(M)
  return((1 / det(M))^(1 / k))
}


#' Criterion function for Ds-Optimality
#'
#' @description
#' Calculates the value of the Ds-Optimality criterion function, which follows the expression:
#' \deqn{\phi_{Ds} = \left(\frac{|M_{22}|}{|M|}\right)^{1/s}}
#'
#'
#' @param M information matrix for which the criterion value wants to be calculated.
#' @param par_int numeric vector with the index of the parameters of interest of the model.
#'
#'
#' @return Numeric value of the Ds-optimality criterion for the information matrix.
dscrit <- function(M, par_int) {
  if (length(M[-par_int, -par_int]) == 1) {
    return((M[-par_int, -par_int] / det(M))^(1 / length(par_int)))
  }
  else {
    return((det(M[-par_int, -par_int]) / det(M))^(1 / length(par_int)))
  }
}

#' Criterion function for I-Optimality and L-Optimality
#'
#' @description
#' Calculates the value of the I-Optimality criterion function, which follows the expression:
#' \deqn{\phi_I = Tr(M^{-1} \cdot B)}
#'
#'
#' @param M information matrix for which the criterion value wants to be calculated.
#' @param matB matrix of the integral of the information matrix over the interest region. Identity matrix for
#'   A-Optimality.
#'
#'
#' @return Numeric value of the I-optimality criterion for the information matrix.
icrit <- function(M, matB) {
  return(tr(matB %*% solve(M)))
}



# Efficiency-------- -----------------------


#' Efficiency between two Information Matrices
#'
#' @param criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#'   * 'L-Optimality'
#' @param mat1 first information matrix, for the numerator.
#' @param mat2 second information matrix, for the denominator.
#' @param k number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#' @param intPars numeric vector with the index of the parameters of interest of the model. Only for "Ds-Optimality".
#' @param matB matrix of the integral of the information matrix over the interest region. Only for "I-Optimality".
#'
#' @return Efficiency of first design with respect to the second design, as a decimal number.
eff <- function(criterion, mat1, mat2, k = 0, intPars = c(1), matB = NA) {
  if (identical(criterion, "D-Optimality")) {
    if (k == 0) k <- nrow(mat1)
    return((det(mat1) / det(mat2))^(1 / k))
  }
  else if (identical(criterion, "Ds-Optimality")) {
    if (length(mat1[-intPars, -intPars]) == 1) {
      return((mat2[-intPars, -intPars] / det(mat2) / (mat1[-intPars, -intPars] / det(mat1)))^(1 / length(intPars)))
    }
    else {
      return((det(mat2[-intPars, -intPars]) / det(mat2) / (det(mat1[-intPars, -intPars]) / det(mat1)))^(1 / length(intPars)))
    }
  }
  else if (identical(criterion, "A-Optimality")) {
    if (k == 0) k <- nrow(mat1)
    return(tr(diag(k) %*% solve(mat2)) / tr(diag(k) %*% solve(mat1)))
  }
  else if (identical(criterion, "I-Optimality") || identical(criterion, "L-Optimality")) {
    return(tr(matB %*% solve(mat2)) / tr(matB %*% solve(mat1)))
  }
}


#' Efficiency between optimal design and a user given design
#'
#' @description
#' Takes an optimal design provided from the function \code{opt_des} and a user given design and compares their
#' efficiency
#'
#' @seealso opt_des
#'
#' @param opt_des_obj an object given by the function \code{opt_des}.
#' @param design dataframe that represents the design. Must have two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#'
#' @return The efficiency as a value between 0 and 1
#' @export
#'
#' @examples
#' result <- opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
#' design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1 / 3, 1 / 3, 1 / 3))
#' design_efficiency(design, result)
design_efficiency <- function(design, opt_des_obj) {
  # check_efficiency_input(opt_des_obj, design) COMPROBAR QUE EL NUMERO DE POUNTOS ES >= LENGTH GRAD/NROW MAT
  if("optdes" %in% class(design)){
    mat1 <- inf_mat(attr(opt_des_obj, "gradient"), design$optdes)
  } else if("data.frame" %in% class(design)){
    mat1 <- inf_mat(attr(opt_des_obj, "gradient"), design)
  } else {
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " The arguments must be two optdes objects or a data.frame and an optdes object")
    stop(error_msg, call. = FALSE)
  }
  if("optdes" %in% class(opt_des_obj)){
    mat2 <- inf_mat(attr(opt_des_obj, "gradient"), opt_des_obj$optdes)
  } else{
    error_msg <- paste0(error_msg, "\n", crayon::red(cli::symbol$cross), " The arguments must be two optdes objects or a data.frame and an optdes object")
    stop(error_msg, call. = FALSE)
  }
  if (identical(opt_des_obj$criterion, "D-Optimality")) {
    eff <- (det(mat1) / det(mat2))^(1 / attr(opt_des_obj, "hidden_value"))
    message(crayon::blue(cli::symbol$info), " The efficiency of the design is ", eff * 100, "%")
    return(eff)
  }
  else if (identical(opt_des_obj$criterion, "Ds-Optimality")) {
    int_pars <- attr(opt_des_obj, "hidden_value")
    if (length(mat1[-int_pars, -int_pars]) == 1) {
      eff <- (mat2[-int_pars, -int_pars] / det(mat2) / (mat1[-int_pars, -int_pars] / det(mat1)))^(1 / length(int_pars))
      message(crayon::blue(cli::symbol$info), " The efficiency of the design is ", eff * 100, "%")
      return(eff)
    }
    else {
      eff <- (det(mat2[-int_pars, -int_pars]) / det(mat2) / (det(mat1[-int_pars, -int_pars]) / det(mat1)))^(1 / length(int_pars))
      message(crayon::blue(cli::symbol$info), " The efficiency of the design is ", eff * 100, "%")
      return(eff)
    }
  }
  else if (identical(opt_des_obj$criterion, "A-Optimality")  || identical(opt_des_obj$criterion, "I-Optimality")  || identical(opt_des_obj$criterion, "L-Optimality")) {
    eff <- tr(attr(opt_des_obj, "hidden_value") %*% solve(mat2)) / tr(attr(opt_des_obj, "hidden_value") %*% solve(mat1))
    message(crayon::blue(cli::symbol$info), " The efficiency of the design is ", eff * 100, "%")
    return(eff)
  }
}
