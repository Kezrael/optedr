# Gradient Vector ------------------------------------


#' Gradient function
#'
#' @description
#' Calculates the gradient function of a \code{model} with respect to the parameters, \code{char_vars}, evaluates
#' it at the provided \code{values} and returns the result as a function of the variable \code{x}.
#'
#' @param model formula describing the model, which must contain only \code{x}, the parameters defined in
#'   \code{char_vars} and the numerical operators.
#' @param char_vars character vector of the parameters of the model.
#' @param values numeric vector with the nominal values of the parameters in \code{char_vars}.
#' @param weight_fun optional function variable that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#'
#' @return A function depending on \code{x} that's the gradient of the \code{model} with respect to \code{char_vars}
gradient <- function(model, char_vars, values, weight_fun = function(x) 1) {
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  ext_char_vars <- c(char_vars, "x")
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f <- as.function(append(stats::setNames(arglist, ext_char_vars), quote({})))
  f1 <- stats::deriv(model, char_vars, f)
  f2 <- function(x_val) {
    attr(do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  f3 <- function(x){
    return(f2(x)*weight_fun(x))
  }
  return(f3)
}


# Information Matrix ------------------------------------


#' Information Matrix
#'
#' @description
#' Given the gradient vector of a model in a single variable model and a design, calculates the information matrix.
#'
#' @param grad A function in a single variable that returns the partial derivatives vector of the model.
#' @param design A dataframe that represents the design. Must have two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#'
#' @return The information matrix of the design, a \eqn{k\times k} matrix where k is the length of the gradient.
inf_mat <- function(grad, design) {
  matrix_ret <- 0 * diag(length(grad(design$Point[[1]])))
  for (i in seq_along(design$Weight)) {
    f_col <- as.matrix(grad(design$Point[[i]]), nrow = 1, byrow = TRUE, dimnames = NULL)
    matrix_ret <- matrix_ret + (t(f_col) %*% f_col) * design$Weight[[i]]
  }
  return(matrix_ret)
}

# Sensibility Function ------------------------------------

#' Master function to calculate the sensitivity function
#'
#' @description
#' Calculates the sensitivity function given the desired \code{Criterion}, an information matrix and other
#' necessary values depending on the chosen criterion.
#'
#' @param Criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#'   * 'L-Optimality'
#' @param grad A function in a single variable that returns the partial derivatives vector of the model.
#' @param M Information Matrix for the sensitivity function.
#' @param par_int Numeric vector of the indexes of the parameters of interest for Ds-Optimality.
#' @param matB Matrix resulting from the integration of the one-point Information Matrix along the interest
#'   region or lineal matrix for L-Optimality.
#'
#' @return The sensitivity function as a matrix of single variable.
sens <- function(Criterion, grad, M, par_int = c(1), matB = NA) {
  if (identical(Criterion, "D-Optimality")) {
    return(dsens(grad, M))
  }
  else if (identical(Criterion, "Ds-Optimality")) {
    return(dssens(grad, M, par_int))
  }
  else if (identical(Criterion, "A-Optimality")) {
    return(isens(grad, M, diag(nrow(M))))
  }
  else if (identical(Criterion, "I-Optimality") || identical(Criterion, "L-Optimality")) {
    return(isens(grad, M, matB))
  }
}


#' Sensitivity function for D-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector and the Identity Matrix.
#'
#' @inherit sens params return
#'
dsens <- function(grad, M) {
  invMat <- solve(M)
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col))
  }
}


#' Sensitivity function for Ds-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector, the Identity Matrix and the parameters of
#' interest.
#'
#' @inherit sens params return
#'
dssens <- function(grad, M, par_int) {
  invMat <- solve(M)
  if (length(M[-par_int, -par_int]) == 1) {
    invMat22 <- 1 / M[-par_int, -par_int]
  } else {
    invMat22 <- solve(M[-par_int, -par_int])
  }
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col) - f_col[-par_int] %*% invMat22 %*% as.matrix(f_col[-par_int], ncol = 1))
  }
}


#' Sensitivity function for I-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector, the Information Matrix and the integral of the
#' one-point Identity Matrix over the interest region. If instead the identity matrix is used, it can be used
#' for A-Optimality.
#'
#' @inherit sens params return
#'
isens <- function(grad, M, matB) {
  invMat <- solve(M)
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% matB %*% invMat %*% t(f_col))
  }
}
