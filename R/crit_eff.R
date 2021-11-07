# Optimality Criteria -----------------------

#' Master function for the criterion function
#'
#' @description
#' Depending on the Criterion input, the function returns the output of the corresponding criterion function given
#' the information matrix.
#'
#' @param Criterion Character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param M Information matrix for which the criterion value wants to be calculated.
#' @param k Numeric number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#' @param par_int Numeric vector with the index of the parameters of interest of the model. Only for "Ds-Optimality".
#' @param matB Matrix of the integral of the information matrix over the interest region. Only for "I-Optimality".
#'
#' @return Numeric value of the optimality criterion for the information matrix.
#'
#'
#' @examples
#' M <- matrix(c(1, 0.75, 0.75, 0.625), nrow = 2)
#' optedr:::crit("D-Optimality", M, k = 2)
crit <- function(Criterion, M, k = 0, par_int = c(1), matB = NA) {
  if (identical(Criterion, "D-Optimality")) {
    return(dcrit(M, k))
  }
  else if (identical(Criterion, "Ds-Optimality")) {
    return(dscrit(M, par_int))
  }
  else if (identical(Criterion, "A-Optimality")) {
    return(icrit(M, diag(k)))
  }
  else if (identical(Criterion, "I-Optimality")) {
    return(icrit(M, matB))
  }
}


#' Criterion function for D-Optimality
#'
#' @description
#' Calculates the value of the D-Optimality criterion, which follows the expression:
#' \deqn{\phi_D = \frac{1}{|M|}^{1/k}}
#'
#'
#' @param M Information matrix for which the criterion value wants to be calculated.
#' @param k Numeric number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#'
#'
#' @return numeric value of the D-optimality criterion for the information matrix.
#'
#' @examples
#' optedr:::dcrit(matrix(c(1, 0.75, 0.75, 0.625), nrow = 2), k = 2)
dcrit <- function(M, k) {
  if (k == 0) k <- nrow(M)
  return((1 / det(M))^(1 / k))
}


#' Criterion function for Ds-Optimality
#'
#' @description
#' Calculates the value of the Ds-Optimality criterion, which follows the expression:
#' \deqn{\phi_D = \frac{|M_{22}|}{|M|}^{1/s}}
#'
#'
#' @param M Information matrix for which the criterion value wants to be calculated.
#' @param par_int Numeric vector with the index of the parameters of interest of the model.
#'
#'
#' @return Numeric value of the Ds-optimality criterion for the information matrix.
#'
#' @examples
#' optedr:::dscrit(matrix(c(1, 0.75, 0.75, 0.625), nrow = 2), c(2))
dscrit <- function(M, par_int) {
  if (length(M[-par_int, -par_int]) == 1) {
    return((M[-par_int, -par_int] / det(M))^(1 / length(par_int)))
  }
  else {
    return((det(M[-par_int, -par_int]) / det(M))^(1 / length(par_int)))
  }
}

#' Criterion function for I-Optimality
#'
#' @description
#' Calculates the value of the Ds-Optimality criterion, which follows the expression:
#' \deqn{\phi_D = \frac{|M_{22}|}{|M|}^{1/s}}
#'
#'
#' @param M Information matrix for which the criterion value wants to be calculated.
#' @param matB Matrix of the integral of the information matrix over the interest region. Identity matrix for
#'   A-Optimality.
#'
#'
#' @return Numeric value of the Ds-optimality criterion for the information matrix.
#'
#' @examples
#' optedr:::icrit(matrix(c(1, 0.75, 0.75, 0.625), nrow = 2), diag(2))
icrit <- function(M, matB) {
  return(tr(matB %*% solve(M)))
}



# Efficiency-------- -----------------------


#' Efficiency between two Information Matrices
#'
#' @param Criterion Character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param mat1 First information matrix, for the numerator.
#' @param mat2 Second information matrix, for the denominator.
#' @param k Number of parameters of the model. Taken from the number of rows of the matrix if omitted.
#' @param intPars Numeric vector with the index of the parameters of interest of the model. Only for "Ds-Optimality".
#' @param matB Matrix of the integral of the information matrix over the interest region. Only for "I-Optimality".
#'
#' @return Efficiency of first design with respect to the second design, as a decimal number.
#'
#' @examples
#' optedr:::eff("D-Optimality", matrix(c(1, 0.75, 0.75, 0.625), nrow = 2),
#' matrix(c(1, 0.25, 0.25, 0.125), nrow = 2))
eff <- function(Criterion, mat1, mat2, k = 0, intPars = c(1), matB = NA) {
  if (identical(Criterion, "D-Optimality")) {
    if (k == 0) k <- nrow(mat1)
    return((det(mat1) / det(mat2))^(1 / k))
  }
  else if (identical(Criterion, "Ds-Optimality")) {
    if (length(mat1[-intPars, -intPars]) == 1) {
      return((mat2[-intPars, -intPars] / det(mat2) / (mat1[-intPars, -intPars] / det(mat1)))^(1 / length(intPars)))
    }
    else {
      return((det(mat2[-intPars, -intPars]) / det(mat2) / (det(mat1[-intPars, -intPars]) / det(mat1)))^(1 / length(intPars)))
    }
  }
  else if (identical(Criterion, "A-Optimality")) {
    if (k == 0) k <- nrow(mat1)
    return(tr(diag(k) %*% solve(mat2)) / tr(diag(k) %*% solve(mat1)))
  }
  else if (identical(Criterion, "I-Optimality")) {
    return(tr(matB %*% solve(mat2)) / tr(matB %*% solve(mat1)))
  }
}


#' Efficiency between optimal design and a user given design
#'
#' @description
#' Takes a optimal design provided from the function \code{opt_des} and a user given design and compares their
#' efficiency
#'
#' @seealso opt_des
#'
#' @param opt_des_obj An object given by the function \code{opt_des}.
#' @param design A dataframe that represents the design. Must have two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#'
#' @return The efficiency as a value between 0 and 1
#' @export
#'
#' @examples
#' result <- opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
#' design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1 / 3, 1 / 3, 1 / 3))
#' design_efficiency(result, design)
design_efficiency <- function(opt_des_obj, design) {
  # check_efficiency_input(opt_des_obj, design) COMPROBAR QUE EL NUMERO DE POUNTOS ES >= LENGTH GRAD/NROW MAT
  mat1 <- inf_mat(attr(opt_des_obj, "gradient"), design)
  mat2 <- inf_mat(attr(opt_des_obj, "gradient"), opt_des_obj$optdes)
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
  else if (identical(opt_des_obj$criterion, "I-Optimality")) {
    eff <- tr(attr(opt_des_obj, "hidden_value") %*% solve(mat2)) / tr(attr(opt_des_obj, "hidden_value") %*% solve(mat1))
    message(crayon::blue(cli::symbol$info), " The efficiency of the design is ", eff * 100, "%")
    return(eff)
  }
}
