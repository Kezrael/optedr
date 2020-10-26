# Gradient Vector ------------------------------------


gradient <- function(model, char_vars, values)
{
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  ext_char_vars <- c(char_vars, "x")
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f <- as.function(append(setNames(arglist, ext_char_vars), quote({})))
  f1 <- deriv(model, char_vars, f)
  f2 <- function(x_val){
    attr( do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  return(f2)
}


# Information Matrix ------------------------------------


inf_mat <- function(grad, design){
  matrix_ret <- 0*diag(length(grad(design$Point[[1]])))
  # USE MAP???
  for(i in seq_along(design$Weight)){
    f_col <- as.matrix(grad(design$Point[[i]]), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    matrix_ret <- matrix_ret + (t(f_col) %*% f_col)*design$Weight[[i]]
  }
  return(matrix_ret)
}

# Sensibility Function ------------------------------------



sens <- function(Criterion, grad, M, intPars = 0, matB = diag(3)){
  if(identical(Criterion, "D-Optimality")){
    return(dsens(grad, M))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(DSsens(grad, M, intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(Isens(grad, M, matB))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(Isens(grad, M, matB))
  }
}


dsens <- function(grad, M){
  invMat <- solve(M)
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col))
  }
}


dssens <- function(grad, M, intPars){
  invMat <- solve(M)
  if(length(M[-intPars, -intPars]) == 1){
    invMat22 <- 1/M[-intPars, -intPars]
  } else {
    invMat22 <- solve(M[[-intPars, -intPars]])
  }
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col) - f_col[-intPars] %*% invMat22 %*% t(f_col[-intPars]))
  }
}


isens <- function(grad, M, matB){
  invMat <- solve(M)
  sens_ret <- function(xval){
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% matB %*% invMat %*%t(f_col))
  }
}



