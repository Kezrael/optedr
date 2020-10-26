# Optimality Criteria -----------------------
# Calcula la función criterio para la matriz de información determinada en función del criterio
# Llama a las funciones particulares
crit <- function(Criterion, mat, k = 3, intPars = 2, matB = NA){
  if(identical(Criterion, "D-Optimality")){
    return(dcrit(mat, k))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(dscrit(mat, length(intPars), intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(icrit(mat, diag(k)))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(icrit(mat, matB))
  }
}

# Calcula la función criterio para D-opt
dcrit <- function(mat, k) {
  return((1/det(mat))^(1/k))
}

# Valor del criterio de Ds-Optimalidad para la matriz
dscrit <- function(mat, s, intPars) {
  if(length(mat[-intPars, -intPars]) == 1){
    return((mat[-intPars, -intPars]/det(mat))^(1/s))
  }
  else{
    return((det(mat[-intPars, -intPars])/det(mat))^(1/s))
  }
}

icrit <- function(mat, matB = diag(3)) {
  return(tr(matB %*% solve(mat)))
}


# Efficiency-------- -----------------------

# Calcula la eficiencia entre dos diseños a partir de las matrices de información dependiendo del criterio
# Llama a las funciones particulares
eff <- function(Criterion, mat1, mat2, k = 3, intPars = 2, matB = NA){
  if(identical(Criterion, "D-Optimality")){
    return(deff(mat1, mat2, k))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(dseff(mat1, mat2, length(intPars), intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(ieff(mat1, mat2, diag(k)))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(ieff(mat1, mat2, matB))
  }
}

# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
deff <- function(mat1, mat2, k) {
  return((det(mat1)/det(mat2))^(1/k))
}

# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
dseff <- function(mat1, mat2, s, intPars) {
  if(length(mat1[-intPars, -intPars]) == 1){
    return((mat2[-intPars, -intPars]/det(mat2)/(mat1[-intPars, -intPars]/det(mat1)))^(1/s))
  }
  else {
    return((det(mat2[-intPars, -intPars])/det(mat2)/(det(mat1[-intPars, -intPars])/det(mat1)))^(1/s))
  }
}

# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
ieff <- function(mat1, mat2, matB = NA){
  return(tr(matB %*% solve(mat2))/tr(matB %*% solve(mat1)))
}
