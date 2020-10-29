# Gradient Vector ------------------------------------


#' Gradient function
#'
#' @description
#' Calculates the gradient function of a \code{model} with respect to the parameters, \code{char_vars}, evaluates
#' it at the provided \code{values} and returns the result as a function of the variable \code{x}.
#'
#' @param model A formula describing the model, which must contain only \code{x}, the parameters defined in
#'   \code{char_vars} and the numerical operators.
#' @param char_vars A character vector of the parameters of the model.
#' @param values Numeric vector with the nominal values of the parameters in \code{char_vars}.
#'
#' @return A function depending on \code{x} that's the gradient of the \code{model} with respect to \code{char_vars}
#'
#' @examples
#' optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
gradient <- function(model, char_vars, values)
{
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  ext_char_vars <- c(char_vars, "x")
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f <- as.function(append(stats::setNames(arglist, ext_char_vars), quote({})))
  f1 <- stats::deriv(model, char_vars, f)
  f2 <- function(x_val){
    attr( do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  return(f2)
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
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
#' design <- data.frame("Point" = seq(212, 422,length.out = 3), "Weight" = rep(1/3, times = 3))
#' optedr:::inf_mat(grad, design)
inf_mat <- function(grad, design){
  matrix_ret <- 0*diag(length(grad(design$Point[[1]])))
  for(i in seq_along(design$Weight)){
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
#' @param Criterion Character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param grad A function in a single variable that returns the partial derivatives vector of the model.
#' @param M Information Matrix for the sensitivity function.
#' @param intPars Numeric vector of the indexes of the parameters of interest for Ds-Optimality.
#' @param matB Matrix resulting from the integration of the unipunctual Information Matrix along the interest
#'   region.
#'
#' @return The sensitivity function as a matrix of single variable.
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
#' design <- data.frame("Point" = seq(212, 422,length.out = 3), "Weight" = rep(1/3, times = 3))
#' matrix <- optedr:::inf_mat(grad, design)
#' optedr:::sens("D-Optimality", grad, matrix)
sens <- function(Criterion, grad, M, intPars = c(1), matB = NA){
  if(identical(Criterion, "D-Optimality")){
    return(dsens(grad, M))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(dssens(grad, M, intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(isens(grad, M, diag(nrow(M))))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(isens(grad, M, matB))
  }
}


#' Sensitivity function for D-Optimality
#'
#' @description
#' Calculates the sensitiviity function from the gradient vector and the Identitiy Matrix.
#'
#' @inherit sens params return
#'
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
#' design <- data.frame("Point" = seq(212, 422,length.out = 3), "Weight" = rep(1/3, times = 3))
#' matrix <- optedr:::inf_mat(grad, design)
#' optedr:::dsens(grad, matrix)
dsens <- function(grad, M){
  invMat <- solve(M)
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col))
  }
}


#' Sensitivity function for Ds-Optimality
#'
#' @description
#' Calculates the sensitiviity function from the gradient vector, the Identitiy Matrix and the parameters of
#' interest.
#'
#' @inherit sens params return
#'
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
#' design <- data.frame("Point" = seq(212, 422,length.out = 3), "Weight" = rep(1/3, times = 3))
#' matrix <- optedr:::inf_mat(grad, design)
#' optedr:::dssens(grad, matrix, c(1))
dssens <- function(grad, M, intPars){
  invMat <- solve(M)
  if(length(M[-intPars, -intPars]) == 1){
    invMat22 <- 1/M[-intPars, -intPars]
  } else {
    invMat22 <- solve(M[-intPars, -intPars])
  }
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col) - f_col[-intPars] %*% invMat22 %*% as.matrix(f_col[-intPars], ncol = 1))
  }
}


#' Sensitivity function for I-Optimality
#'
#' @description
#' Calculates the sensitiviity function from the gradient vector, the Identitiy Matrix and the integral of the
#' unipunctual Identity Matrix over the interest region. If instead the identity matrix is used, it can be used
#' for A-Optimality.
#'
#' @inherit sens params return
#'
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a*exp(-b/x), c("a", "b"), c(1, 1500))
#' design <- data.frame("Point" = seq(212, 422,length.out = 3), "Weight" = rep(1/3, times = 3))
#' matrix <- optedr:::inf_mat(grad, design)
#' optedr:::isens(grad, matrix, diag(2))
isens <- function(grad, M, matB){
  invMat <- solve(M)
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% matB %*% invMat %*%t(f_col))
  }
}



