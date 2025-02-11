# General Antoine

# Cálculo de la traza de una matriz
tr <- function(m) {
  return(sum(diag(m)))
}


#### HOMOCEDASTICO ----------------------------------------------------------

# Calcular matrix de un diseño para la ecuación de Antoine
dmatrixAntoineHom <- function(a, b, c, design) {
  a <- a*log(10)
  b <- b*log(10)

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  for(i in seq_along(design$Weight)){
    m11 <- m11 + exp(a - b/(c + design$Point[[i]]))*exp(a - b/(c + design$Point[[i]]))*design$Weight[[i]]
    m12 <- m12 + exp(a - b/(c + design$Point[[i]]))*(-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*design$Weight[[i]]
    m13 <- m13 + exp(a - b/(c + design$Point[[i]]))*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
    m21 <- m12
    m22 <- m22 + (-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*(-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*design$Weight[[i]]
    m23 <- m23 + (-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
    m31 <- m13
    m32 <- m23
    m33 <- m33 + (b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
  }

  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}



# design <- data.frame(points = c(1, 50.5, 100), weights = c(1/3, 1/3, 1/3))

# (design2 <- doptAntoine(8.07131, 730.63, 233.426, 1, 100))
# matInit2 <- dmatrixAntoine(a = 8.07131, b = 730.63, c = 233.426, design = design2)

# solve(matInit2)

# matInit <- dmatrixAntoine(a = 8.07131, b = 1730.63, c = 233.426, design = design)


# Función para encontrar el x donde se alcanza el máximo de una función en una rejilla dada, evaluándolo en la misma
findmax <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  xmax <- grid[which.max(map(grid, sens))]
  return(xmax)
}

# findmax(sens, 1, 100, 1000)


# Función para encontrar el mínimo de una función en una rejilla dada, evaluándolo en la misma
findminval <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  minval <- min(map(grid, sens))
  return(minval)
}

# findmax(sens, 1, 100, 1000)



# Función para actualizar un diseño añadiendo un nuevo punto (o peso al mismo si está cerca de otro ya en el diseño)
updateDesign <- function(design, xmax, newalpha, delta){
  design$Weight <- design$Weight*(1-newalpha)
  absdiff <- abs(design$Point - xmax) < delta
  if (any(absdiff))
  {
    pos <- min(which(absdiff == TRUE))
    design$Point[[pos]] <- (design$Point[[pos]]*design$Weight[[pos]] + xmax*newalpha)/(design$Weight[[pos]]+newalpha)
    design$Weight[[pos]] <- design$Weight[[pos]]+newalpha
  }
  else
  {
    design[nrow(design) + 1,] <- c(xmax, newalpha)
  }
  return(design)
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/4, 1/2))

# updateDesign(des, 50, 0.6)

# Actualizar un diseño sin añadir puntos
updateDesignTotal <- function(design, delta){
  updated <- FALSE
  finished <- FALSE
  while(!finished) {
    for(i in 1:(length(design$Point)-1)) {
      absdiff <- abs(design$Point[-seq(1, i)] - design$Point[i]) < delta
      if (any(absdiff)){
        updated <- TRUE
        design <- updateDesign(design[-i,], design$Point[i], design$Weight[i], delta)
        break
      }
    }
    finished <- !updated
    updated <- FALSE
  }
  return(design)
}


# Función para eliminar los puntos con menos de un determinado peso, delta
deletePoints <- function(design, delta) {
  updatedDesign <- design[design$Weight > delta, ]
  updatedDesign$Weight <- updatedDesign$Weight/sum(updatedDesign$Weight)
  return(updatedDesign)
}

# des <- data.frame(Point = c(1, 50.5, 100, 5, 30, 80), Weight = c(1/4, 1/4, 1/4, 1/200, 1/100, 1/500))

# deletePoints(des, 0.005)






# --------------------------- D-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad de la ecuación de Antoine
dsensAntoine <- function(a, b, c, mat) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  sensX <- function(x) {
    # return(exp(a - b/(c + x))*exp(a - b/(c + x))*desMat[[1,1]]+exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[1,2]]+exp(a - b/(c + x))*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[1,3]]+
    #          exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[2,1]]+(-(exp(a - b/(c + x))/(c + x)))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[2,2]]+(-(exp(a - b/(c + x))/(c + x)))*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[2,3]]+
    #          (b*exp(a - b/(c + x)))/(c + x)^2*exp(a - b/(c + x))*desMat[[3,1]]+(b*exp(a - b/(c + x)))/(c + x)^2*(-(exp(a - b/(c + x))/(c + x)))*desMat[[3,2]]+(b*exp(a - b/(c + x)))/(c + x)^2*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[3,3]])
    return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
             (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
             (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2)
  }
  return(sensX)
}


# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# sens(100)

# Cálculo Analítico del diseño D-Óptimo para la ecuación de Antoine
doptAntoineHom <- function(A, B, C, min, max){
  A <- log(10)*A
  B <- log(10)*B

  if(round(min - max, 3) == 0){
    opdes <- data.frame(Point = c(min), Weight = c(1))
    return(opdes)
  }

  T1 <- (-3*C^3+2*(B)^2*max-6*C^2*max-3*B*(C^2-max^2)-3*C*max^2-sqrt(3)*sqrt((B)^2*(C+max)^4))/(2*(B)^2+6*B*(C+max)+3*(C+max)^2)

  if(min <= T1)
  {
    T2 <- (-3*C^3+2*(B)^2*max-6*C^2*max-3*B*(C^2-max^2)-3*C*max^2+sqrt(3)*sqrt((B)^2*(C+max)^4))/(2*(B)^2+6*B*(C+max)+3*(C+max)^2)
    opdes <- data.frame(Point = c(T1, T2, max), Weight = c(1/3, 1/3, 1/3))
  }
  else
  {
    T2 <- (1/(2*(-B - 2*C - max - min)))*(2*C^2 - B*max - B*min - 2*max*min - sqrt((-2*C^2 + B*max + B*min + 2*max*min)^2 - 4*(-B - 2*C - max - min)*(C^2*max + C^2*min - B*max*min + 2*C*max*min)))
    opdes <- data.frame(Point = c(min, T2, max), Weight = c(1/3, 1/3, 1/3))
  }

  return(opdes)
}
# A <- 18.58487808693377
# B <- 1682.3377464942398
# C <- 233.426
#
# min <- 1
# max <- 100
# (1/(2*(-B - 2*C - max - min)))*(2*C^2 - B*max - B*min - 2*max*min - sqrt((-2*C^2 + B*max + B*min + 2*max*min)^2 - 4*(-B - 2*C - max - min)*(C^2*max + C^2*min - B*max*min + 2*C*max*min)))


# typeof(doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 99, 374))

# class(doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 99, 374))
# doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 1, 40)

# doptAntoine(A = 8.14019, B = 1810.94, C = 244.485, 1, 40)



# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
deff <- function(mat1, mat2, k) {
  return((det(mat1)/det(mat2))^(1/k))
}

# dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design)
#
# 5	0.375
# 35	0.3125
# 70	0.3125
#
# deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design),
#      dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)

# Valor del criterio de D-Optimalidad para la matriz
dcrit <- function(mat, k) {
  return((1/det(mat))^(1/k))
}

# dcrit(matInit, 3)


# Función para actualizar los pesos de un diseño para el algoritmo WF
updateWeights <- function(design, sens, k, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/k)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights/sum(weights))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/3, 1/2))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, des)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# (des$Weight <- updateWeights(des, sens, 3, 1/2))



# Algoritmo WF con multiplicativo para los pesos para D-Opt
WFMult <- function(A, B, C, initDes, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHom(A, B, C, initDes)
    critVal[index] <- dcrit(M, k)
    index <- index + 1
    sens <- dsensAntoine(A, B, C, M)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - k)/k) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+k), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- dmatrixAntoineHom(A, B, C, initDes)
      critVal[index] <- dcrit(M, k)
      index <- index + 1
      sens <- dsensAntoine(A, B, C, M)
      initDes$Weight <- updateWeights(initDes, sens, k, deltaW)
      stopw <- any(max(abs(weightsInit - initDes$Weight) < tol)) || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
  }
  critVal[index] <- dcrit(M, k)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/3, 1/2))
# system.time(for(i in 1:100) {WFMult(8.07131, 1730.63, 233.426, des, 1, 100, 1000, 8, 0.07, 3, 5/6, 0.0001)})


# --------------------------- Ds-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad de la ecuación de Antoine
DSsensAntoine <- function(a, b, c, mat, intPars) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  invMat2 <- solve(mat[-intPars, -intPars])
  if(all(intPars == c(1,2))){
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             - (invMat2[1,1]*b^2*exp(2*a - 2*b/(c + x)))/(c + x)^4)
    }
  }
  else if (all(intPars == c(1,3))) {
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             - exp(2*a-2*b/(c+x))*invMat2[1,1]/(c+x)^2)
    }
  }
  else if (all(intPars == c(2,3))) {
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             -exp(2*a-2*b/(c+x))*invMat2[1,1])
    }
  }
  else if (all(intPars == c(1))) {
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             - exp(2*a-2*b/(c+x))*(invMat2[2,2]*b^2+(c+x)*(-invMat2[1,2]*b-invMat2[2,1]*b+invMat2[1,1]*(c+x)))/(c+x)^4)
    }
  }
  else if (all(intPars == c(2))) {
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             - exp(2*a-2*b/(c+x))*(invMat2[2,2]*b^2+(c+x)^2*(invMat2[1,2]*b+invMat2[2,1]*b+invMat2[1,1]*(c+x)^2))/(c+x)^4)
    }
  }
  else if (all(intPars == c(3))) {
    sensX <- function(x) {
      return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
               (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
               (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2
             - exp(2*a-2*b/(c+x))*(invMat2[2,2]+(c+x)*(-invMat2[1,2]-invMat2[2,1]+invMat2[1,1]*(c+x)))/(c+x)^2)
    }
  }
  else {
    sensX <- NULL
  }
  return(sensX)
}

# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)

# sens <- DSsensAntoine(8.07131, 1730.63, 233.426, desMat, c(1))

# sens(50)



# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
dseff <- function(mat1, mat2, s, intPars) {
  if(length(mat1[-intPars, -intPars]) == 1){
    return((mat2[-intPars, -intPars]/det(mat2)/(mat1[-intPars, -intPars]/det(mat1)))^(1/s))
  }
  else {
    return((det(mat2[-intPars, -intPars])/det(mat2)/(det(mat1[-intPars, -intPars])/det(mat1)))^(1/s))
  }
}


# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# optdes <- DSWFMult(8.07131, 1730.63, 233.426, des, c(1,2), 1, 100, 1000, 8, 0.02, 3, 1/2, 0.00001)
# des2 <- data.frame(Point = c(34, 78, 90), Weight = c(1/3, 1/3, 1/3))
# dseff(dmatrixAntoine(8.07131, 1730.63, 233.426, des2),
#      dmatrixAntoine(8.07131, 1730.63, 233.426, optdes), 2, c(1, 2))



# Valor del criterio de D-Optimalidad para la matriz
dscrit <- function(mat, s, intPars) {
  if(length(mat[-intPars, -intPars]) == 1){
    return((mat[-intPars, -intPars]/det(mat))^(1/s))
  }
  else{
    return((det(mat[-intPars, -intPars])/det(mat))^(1/s))
  }
}

# dcrit(matInit, 3)


# Función para actualizar los pesos de un diseño para el algoritmo WF
updateWeightsDS <- function(design, sens, s, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/s)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights)
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
#
# desMat <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, des)
#
# sens <- DSsensAntoineHet(8.07131, 1730.63, 233.426, desMat, c(1,2))
#
# (des$Weight <- updateWeightsDS(des, sens, 2, 1/2))



DSWFMult <- function(A, B, C, initDes, intPar, min, max, grid.length, joinThresh, deleteThresh, s, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHom(A, B, C, initDes)
    critVal[index] <- dscrit(M, s, intPar)
    index <- index + 1
    sens <- DSsensAntoine(A, B, C, M, intPar)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - s)/s) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+3), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      critVal[index] <- dscrit(M, s, intPar)
      index <- index + 1
      M <- dmatrixAntoineHom(A, B, C, initDes)
      sens <- DSsensAntoine(A, B, C, M, intPar)
      initDes$Weight <- updateWeightsDS(initDes, sens, length(intPar), deltaW)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i%%5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  critVal[index] <- dscrit(M, s, intPar)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# system.time(for(i in 1:100) {DSWFMult(8.07131, 1730.63, 233.426, des, c(1,2), 1, 100, 1000, 8, 0.02, 3, 1/2, 0.00001)})
# optdes <- DSWFMult(8.07131, 1730.63, 233.426, des, c(1,2), 1, 100, 1000, 8, 0.02, 3, 1/2, 0.00001)
# optdes$optdes
# M <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes$optdes)
# sens <- DSsensAntoine(8.07131, 1730.63, 233.426, M, c(1,2))


# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) 2, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")

# --------------------------- A- e I-Optimalidad ----------------------------------

# Calcula la integral de la matriz de información en el rango de interés en función de una distribución dada
IntMatAntoineHom <- function(a, b, c, minReg, maxReg, vertex, option = "Unif") {
  a <- a*log(10)
  b <- b*log(10)

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  if(option == "Unif" && maxReg > minReg)
  {
    f11 <- function(x) {exp(a - b/(c + x))*exp(a - b/(c + x))/(maxReg - minReg)}
    f12 <- function(x) {exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))/(maxReg - minReg)}
    f13 <- function(x) {exp(a - b/(c + x))*(b*exp(a - b/(c + x)))/(c + x)^2/(maxReg - minReg)}
    f22 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(-(exp(a - b/(c + x))/(c + x)))/(maxReg - minReg)}
    f23 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(b*exp(a - b/(c + x)))/(c + x)^2/(maxReg - minReg)}
    f33 <- function(x) {(b*exp(a - b/(c + x)))/(c + x)^2*(b*exp(a - b/(c + x)))/(c + x)^2/(maxReg - minReg)}

    m11 <- integrate(f11, lower = minReg, upper = maxReg)$value
    m12 <- integrate(f12, lower = minReg, upper = maxReg)$value
    m13 <- integrate(f13, lower = minReg, upper = maxReg)$value
    m21 <- m12
    m22 <- integrate(f22, lower = minReg, upper = maxReg)$value
    m23 <- integrate(f23, lower = minReg, upper = maxReg)$value
    m31 <- m13
    m32 <- m23
    m33 <- integrate(f33, lower = minReg, upper = maxReg)$value
  }
  else if(option == "Triang")
  {
    f111 <- function(x) {exp(a - b/(c + x))*exp(a - b/(c + x))*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f112 <- function(x) {exp(a - b/(c + x))*exp(a - b/(c + x))*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f121 <- function(x) {exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f122 <- function(x) {exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f131 <- function(x) {exp(a - b/(c + x))*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f132 <- function(x) {exp(a - b/(c + x))*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f221 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(-(exp(a - b/(c + x))/(c + x)))*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f222 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(-(exp(a - b/(c + x))/(c + x)))*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f231 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f232 <- function(x) {(-(exp(a - b/(c + x))/(c + x)))*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f331 <- function(x) {(b*exp(a - b/(c + x)))/(c + x)^2*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f332 <- function(x) {(b*exp(a - b/(c + x)))/(c + x)^2*(b*exp(a - b/(c + x)))/(c + x)^2*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}

    if(vertex > minReg & maxReg > vertex)
    {
      m11 <- integrate(f111, lower = minReg, upper = vertex)$value + integrate(f112, lower = vertex, upper = maxReg)$value
      m12 <- integrate(f121, lower = minReg, upper = vertex)$value + integrate(f122, lower = vertex, upper = maxReg)$value
      m13 <- integrate(f131, lower = minReg, upper = vertex)$value + integrate(f132, lower = vertex, upper = maxReg)$value
      m21 <- m12
      m22 <- integrate(f221, lower = minReg, upper = vertex)$value + integrate(f222, lower = vertex, upper = maxReg)$value
      m23 <- integrate(f231, lower = minReg, upper = vertex)$value + integrate(f232, lower = vertex, upper = maxReg)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f331, lower = minReg, upper = vertex)$value + integrate(f332, lower = vertex, upper = maxReg)$value
    }
    else if(near(vertex, maxReg))
    {
      m11 <- integrate(f111, lower = minReg, upper = vertex)$value
      m12 <- integrate(f121, lower = minReg, upper = vertex)$value
      m13 <- integrate(f131, lower = minReg, upper = vertex)$value
      m21 <- m12
      m22 <- integrate(f221, lower = minReg, upper = vertex)$value
      m23 <- integrate(f231, lower = minReg, upper = vertex)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f331, lower = minReg, upper = vertex)$value
    }
    else
    {
      m11 <- integrate(f112, lower = vertex, upper = maxReg)$value
      m12 <- integrate(f122, lower = vertex, upper = maxReg)$value
      m13 <- integrate(f132, lower = vertex, upper = maxReg)$value
      m21 <- m12
      m22 <- integrate(f222, lower = vertex, upper = maxReg)$value
      m23 <- integrate(f232, lower = vertex, upper = maxReg)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f332, lower = vertex, upper = maxReg)$value
    }

  }



  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}


# IntMatAntoine(8.07131, 1730.63, 233.426, 80, 120, 0, option = "Unif")

# IntMatAntoine(8.07131, 1730.63, 233.426, 98, 102, 100, option = "Triang")

# library(tidyverse)
# IntMatAntoine(8.07131, 1730.63, 233.426, 98, 100, 100, option = "Triang")


# Función de sensibilidad para I-Optimalidad de la ecuación de Antoine
IsensAntoine <- function(a, b, c, mat, matB = diag(3)) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  sensMat <- solve(mat) %*% matB %*% solve(mat)
  sensX <- function(x) {
  return(exp(a - b/(c + x))*(sensMat[[1,1]]*exp(a - b/(c + x)) + (sensMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (sensMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
             (exp(a - b/(c + x))*(sensMat[[1,2]]*exp(a - b/(c + x)) + (sensMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (sensMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
             (b*exp(a - b/(c + x))*(sensMat[[1,3]]*exp(a - b/(c + x)) + (sensMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (sensMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2)
  }
  return(sensX)
}

# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
#
# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)
#
# sens <- IsensAntoine(8.07131, 1730.63, 233.426, desMat)
#
# sens(70)/10^8

# B <- IntMatAntoine(8.07131, 1730.63, 233.426, 80, 120, 0, option = "Unif")
#
# optdes <- data.frame(Point = c(100, 83.9316, 33.2345), Weight = c(0.359629, 0.377516, 0.262233))
#
# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)
#
# sens <- IsensAntoine(8.07131, 1730.63, 233.426, desMat, B)
#
# sens(70)
#
# crit <- icrit(desMat, B)
#
# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) crit, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")




# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
ieff <- function(mat1, mat2, matB = diag(3)) {
  return(tr(matB %*% solve(mat2))/tr(matB %*% solve(mat1)))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# optdes <- IWFMult(8.07131, 1730.63, 233.426, des, diag(3),1, 100, 1000, 5, 0.01, 1/2, 0.00001)
# des2 <- data.frame(Point = c(34, 78, 90), Weight = c(1/3, 1/3, 1/3))
# ieff(dmatrixAntoine(8.07131, 1730.63, 233.426, des2),
#       dmatrixAntoine(8.07131, 1730.63, 233.426, optdes), diag(3))



# Valor del criterio de D-Optimalidad para la matriz
icrit <- function(mat, matB = diag(3)) {
  return(tr(matB %*% solve(mat)))
}

# icrit(matInit, diag(3))


# Función para actualizar los pesos de un diseño para el algoritmo WF
updateWeightsI <- function(design, sens, crit, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/crit)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights)
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/3, 1/2))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, des)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# (des$Weight <- updateWeights(des, sens, 3, 1/2))



IWFMult <- function(A, B, C, initDes, matB = diag(3), min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHom(A, B, C, initDes)
    critVal[index] <- icrit(M, matB)
    index <- index + 1
    sens <- IsensAntoine(A, B, C, M, matB)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - critVal[index-1])/critVal[index-1]) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+3), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- dmatrixAntoineHom(A, B, C, initDes)
      crit <- icrit(M, matB)
      critVal[index] <- crit
      index <- index + 1
      sens <- IsensAntoine(A, B, C, M, matB)
      initDes$Weight <- updateWeightsI(initDes, sens, crit, deltaW)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i%%5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  M <- dmatrixAntoineHom(A, B, C, initDes)
  critVal[index] <- icrit(M, matB)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# matUnif <- IntMatAntoineHom(8.07131, 1730.63, 233.426, 70, 100, 100, option = "Unif")
# matTriang <- IntMatAntoineHom(8.07131, 1730.63, 233.426, 70, 100, 100, option = "Triang")
# optdes <- IWFMult(8.07131, 1730.63, 233.426, des, matUnif, 1, 100, 1000, 9, 0.01, 1/2, 0.00001)
# optdes <- IWFMult(8.07131, 1730.63, 233.426, des, matTriang, 1, 100, 1000, 9, 0.01, 1/2, 0.00001)

# system.time(for(i in 1:100) {IWFMult(8.07131, 1730.63, 233.426, des, diag(3), 1, 100, 1000, 9, 0.01, 1/2, 0.00001)})
# optdes$optdes
# optdes$convergence
# M <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)
# sens <- IsensAntoine(8.07131, 1730.63, 233.426, M)
# crit <- icrit(M)

# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) crit, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")


# ______________________________________________________

# López-Ríos

# Calculate boundaries for given delta
delta_bound <- function(delta, k, sens_min){
  min <- -(-1 + delta)*((-1 + delta - sens_min*delta)/(-1 + delta))^(1/k)
  max <- -(-1 + delta)*((-1 + delta - k*delta)/(-1 + delta))^(1/k)
  return(c(min, max))
}

# delta_bound(1/4, 3, -2.50965)


# NO SE PUEDE "FÁCIL"
# Calculate boundary for given efficiency
# efficiency_bound <- function(deff, k, sens_min){
#   min <- -(-1 + delta)*((-1 + delta - sens_min*delta)/(-1 + delta))^(1/k)
#   max <- -(-1 + delta)*((-1 + delta - k*delta)/(-1 + delta))^(1/k)
#   return(c(min, max))
# }



# Calculate sens value for given delta and efficiency
sens_val_to_add <- function(deff, delta, k){
  return((1 - delta)/delta*((deff/(1 - delta))^k - 1))
}

# sens_val_to_add(0.9, 1/4, 3)


# Elimina puntos duplicados
update_sequence <- function(points, tol){
  i <- 1
  imax <- (length(points)-1)
  while(i < imax) {
    absdiff <- c(rep(T, i), abs(points[-seq(1, i)] - points[i]) > tol)
    points <- points[absdiff]
    i <- i+1
    imax <- (length(points)-1)
  }
  return(points)
}




# Calculate crosspoints
crosspoints <- function(deff, delta, k, sens, gridlength, tol, xmin, xmax){
  val <- sens_val_to_add(deff, delta, k)

  sensfix <- function(x){
    return(sens(x) - val)
  }

  sols <- seq(xmin, xmax, length.out = gridlength) %>%
    map(nleqslv, fn = sensfix) %>%
    map(function(x) x$x) %>%
    unlist

  # Eliminar duplicados
  sols_upd <- update_sequence(sols, tol)

  # Quedarnos con puntos dentro del espacio de diseño
  sols_upd <- sols_upd[sols_upd >= 1 & sols_upd <= 100]

  # Eliminar los que no son soluciones
  sols_upd <- sols_upd[abs(map_dbl(sols_upd, sens) - val) < tol]


  return(sols_upd)
}

# optdes <- doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 1, 100)

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# sols_new <- crosspoints(0.9, 1/4, 3, sens, 1000, 10^(-3), 1, 100)




# Update design given crosspoints and delta
add_points <- function(points, delta, design){
  new_points <- data.frame("Point" = points, "Weight" = rep(delta/length(points), times = length(points)))
  design["Weight"] <- design["Weight"]*(1-delta)
  return(rbind(design, new_points))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# add_points(c(20, 70, 90), 1/4, des)


#-----------------------------


# Uniform design
# uniform_design <- function(n, xmin, xmax){
#   unif_des <- data.frame("Point" = seq(xmin,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#   return(unif_des)
# }

# Arithmetic design
# arithmetic_design <- function(Variance, n, xmin, xmax, opt_mat, Criterion, a = 0, b = 0, c = 0, k = 3, s = 2, intPars = 2, matB = diag(3)){
#   arithdes_opt <- function(r){
#     arith_des <- data.frame("Point" = seq(r,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#     arith_mat <- dmatrixAntoine(Variance, a, b, c, arith_des)
#     return(eff(Criterion, opt_mat, arith_mat, k, s, intPars, matB))
#   }
#
#   r_val <- round(nloptr::direct(fn = arithdes_opt,lower = xmin, upper = xmax, control=list(xtol_rel=1e-24, maxeval=1000))$par, 3)
#
#   return(data.frame("Point" = seq(r_val,xmax,length.out = n), "Weight" = rep(1/n, times = n)))
# }

# opt_des <- doptCuad(1, 100)
# dopt_mat <- dmatrixCuad(opt_des)
#
# arithmetic_design(5, 1, 100, dopt_mat, "Cuadratic")


# Geometric design
# geometric_design <- function(Variance, n, xmin, xmax, opt_mat, Criterion, a = 0, b = 0, c = 0, k = 3, s = 2, intPars = 2, matB = diag(3)){
#
#   geodes_opt <- function(r){
#     geom_des <- data.frame("Point" = (xmax-xmin)*r^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n))
#     geom_mat <- dmatrixAntoine(Variance, a, b, c, geom_des)
#     return(eff(Criterion, opt_mat, geom_mat, k, s, intPars, matB))
#   }
#
#   r_val <- round(nloptr::direct(fn = geodes_opt,lower = 0, upper = 1, control=list(xtol_rel=1e-8, maxeval=1000))$par, 3)
#
#   return(data.frame("Point" = (xmax-xmin)*r_val^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n)))
# }





#### HETEROCEDASTICO ----------------------------------------------------------

# Calcular matrix de un diseño para la ecuación de Antoine
dmatrixAntoineHet <- function(a, b, c, design) {
  b <- b*log(10)

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  for(i in seq_along(design$Weight)){
    m11 <- m11 + design$Weight[[i]]
    m12 <- m12 + -1/(c + design$Point[[i]])*design$Weight[[i]]
    m13 <- m13 + b/(c + design$Point[[i]])^2*design$Weight[[i]]
    m21 <- m12
    m22 <- m22 + 1/(c + design$Point[[i]])^2*design$Weight[[i]]
    m23 <- m23 + -b/(c + design$Point[[i]])^3*design$Weight[[i]]
    m31 <- m13
    m32 <- m23
    m33 <- m33 + b^2/(c + design$Point[[i]])^4*design$Weight[[i]]
  }

  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}



# design <- data.frame(points = c(1, 50.5, 100), weights = c(1/3, 1/3, 1/3))

# (design2 <- doptAntoine(8.07131, 730.63, 233.426, 1, 100))
# matInit2 <- dmatrixAntoine(a = 8.07131, b = 730.63, c = 233.426, design = design2)

# solve(matInit2)

# matInit <- dmatrixAntoine(a = 8.07131, b = 1730.63, c = 233.426, design = design)




# --------------------------- D-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad de la ecuación de Antoine
dsensAntoineHet <- function(a, b, c, mat) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  sensX <- function(x) {
    return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
             (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
             b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2)
  }
  return(sensX)
}


# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
#
# desMat <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, optdes)
#
# sens <- dsensAntoineHet(8.07131, 1730.63, 233.426, desMat)
#
# sens(1)

# Cálculo Analítico del diseño D-Óptimo para la ecuación de Antoine
doptAntoineHet <- function(A, B, C, min, max){
  A <- log(10)*A
  B <- log(10)*B

  if(round(min - max, 3) == 0){
    opdes <- data.frame(Point = c(min), Weight = c(1))
    return(opdes)
  }
  T2 <- (C*max+C*min+2*min*max)/(2*C+max+min)
  opdes <- data.frame(Point = c(min, T2, max), Weight = c(1/3, 1/3, 1/3))

  return(opdes)
}
# A <- 18.58487808693377
# B <- 1682.3377464942398
# C <- 233.426
#
# min <- 1
# max <- 100


# class(doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 99, 374))
# doptAntoineHet(A = 8.07131, B = 1730.63, C = 233.426, 1, 100)

# doptAntoine(A = 8.14019, B = 1810.94, C = 244.485, 1, 40)




# Algoritmo WF con multiplicativo para los pesos para D-Opt
WFMultHet <- function(A, B, C, initDes, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHet(A, B, C, initDes)
    critVal[index] <- dcrit(M, k)
    index <- index + 1
    sens <- dsensAntoineHet(A, B, C, M)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - k)/k) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+k), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- dmatrixAntoineHet(A, B, C, initDes)
      critVal[index] <- dcrit(M, k)
      index <- index + 1
      sens <- dsensAntoineHet(A, B, C, M)
      initDes$Weight <- updateWeights(initDes, sens, k, deltaW)
      stopw <- any(max(abs(weightsInit - initDes$Weight) < tol)) || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
  }
  critVal[index] <- dcrit(M, k)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/3, 1/2))
# system.time(for(i in 1:100) {WFMult(8.07131, 1730.63, 233.426, des, 1, 100, 1000, 8, 0.07, 3, 5/6, 0.0001)})


# --------------------------- Ds-Optimalidad ----------------------------------

# Función de sensibilidad para Ds-Optimalidad de la ecuación de Antoine
DSsensAntoineHet <- function(a, b, c, mat, intPars) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  invMat2 <- solve(mat[-intPars, -intPars])
  if(all(intPars == c(1,2))){
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 - invMat2[[1,1]]*b^2/(c+x)^4)
    }
  }
  else if (all(intPars == c(1,3))) {
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 - invMat2[[1,1]]/(c+x)^2)
    }
  }
  else if (all(intPars == c(2,3))) {
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 - invMat2[[1,1]])
    }
  }
  else if (all(intPars == c(1))) {
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 + (b*invMat2[[1,2]]/(c+x)^2 - invMat2[[1,1]]/(c+x))/(c+x) - b*(b*invMat2[[2,2]]/(c+x)^2 - invMat2[[1,2]]/(c+x))/(c+x)^2)
    }
  }
  else if (all(intPars == c(2))) {
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 - invMat2[[1,1]] - b*invMat2[[1,2]]/(c+x)^2 - b*(invMat2[[1,2]]+b*invMat2[[2,2]]/(c+x)^2)/(c+x)^2)
    }
  }
  else if (all(intPars == c(3))) {
    sensX <- function(x) {
      return(invMat[[1,1]] + invMat[[3,1]]*b/(c + x)^2 - (invMat[[2,1]]/(c + x)) -
               (invMat[[1,2]] + invMat[[3,2]]*b/(c + x)^2 - invMat[[2,2]]/(c + x))/(c + x) +
               b*(invMat[[1,3]] + invMat[[3,3]]*b/(c + x)^2 - invMat[[2,3]]/(c + x))/(c + x)^2 - invMat2[[1,1]] + invMat2[[1,2]]/(c+x) + (invMat2[[1,2]] - invMat2[[2,2]]/(c+x))/(c+x))
    }
  }
  else {
    sensX <- NULL
  }
  return(sensX)
}

# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
#
# desMat <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, optdes)
#
# sens <- DSsensAntoineHet(8.07131, 1730.63, 233.426, desMat, c(2))
#
# sens(50)



DSWFMultHet <- function(A, B, C, initDes, intPar, min, max, grid.length, joinThresh, deleteThresh, s, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHet(A, B, C, initDes)
    critVal[index] <- dscrit(M, s, intPar)
    index <- index + 1
    sens <- DSsensAntoineHet(A, B, C, M, intPar)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - s)/s) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+3), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      critVal[index] <- dscrit(M, s, intPar)
      index <- index + 1
      M <- dmatrixAntoineHet(A, B, C, initDes)
      sens <- DSsensAntoineHet(A, B, C, M, intPar)
      initDes$Weight <- updateWeightsDS(initDes, sens, length(intPar), deltaW)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i%%5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  critVal[index] <- dscrit(M, s, intPar)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# initDes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# system.time(for(i in 1:100) {DSWFMultHet(8.07131, 1730.63, 233.426, des, c(1,2), 1, 100, 1000, 8, 0.02, 3, 1/2, 0.00001)})
# optdes <- DSWFMultHet(8.07131, 1730.63, 233.426, des, c(1), 1, 100, 1000, 8, 0.05, 1, 5/6, 0.00001)

# arrange(optdes$optdes, Point)
# M <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, optdes$optdes)
# sens <- DSsensAntoineHet(8.07131, 1730.63, 233.426, M, c(1))


# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) 2, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")

# --------------------------- A- e I-Optimalidad ----------------------------------

# Calcula la integral de la matriz de información en el rango de interés en función de una distribución dada
IntMatAntoineHet <- function(a, b, c, minReg, maxReg, vertex, option = "Unif") {
  a <- a*log(10)
  b <- b*log(10)

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0
  if(option == "Unif" && maxReg > minReg)
  {
    # f11 <- function(x) {1/(maxReg - minReg)}
    f12 <- function(x) {(-1/(c + x))/(maxReg - minReg)}
    f13 <- function(x) {b/(c + x)^2/(maxReg - minReg)}
    f22 <- function(x) {1/(c + x)^2/(maxReg - minReg)}
    f23 <- function(x) {-b/(c + x)^3/(maxReg - minReg)}
    f33 <- function(x) {b^2/(c + x)^4/(maxReg - minReg)}

    # m11 <- integrate(f11, lower = minReg, upper = maxReg)$value
    m11 <- 1
    m12 <- integrate(f12, lower = minReg, upper = maxReg)$value
    m13 <- integrate(f13, lower = minReg, upper = maxReg)$value
    m21 <- m12
    m22 <- integrate(f22, lower = minReg, upper = maxReg)$value
    m23 <- integrate(f23, lower = minReg, upper = maxReg)$value
    m31 <- m13
    m32 <- m23
    m33 <- integrate(f33, lower = minReg, upper = maxReg)$value
  }
  else if(option == "Triang")
  {
    # f111 <- function(x) {1*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    # f112 <- function(x) {1*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f121 <- function(x) {(-1/(c + x))*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f122 <- function(x) {(-1/(c + x))*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f131 <- function(x) {b/(c + x)^2*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f132 <- function(x) {b/(c + x)^2*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f221 <- function(x) {1/(c + x)^2*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f222 <- function(x) {1/(c + x)^2*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f231 <- function(x) {-b/(c + x)^3*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f232 <- function(x) {-b/(c + x)^3*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}
    f331 <- function(x) {b^2/(c + x)^4*(2*(x - minReg)/((maxReg - minReg)*(vertex - minReg)))}
    f332 <- function(x) {b^2/(c + x)^4*(2*(maxReg - x)/((maxReg - minReg)*(maxReg - vertex)))}

    if(vertex > minReg & maxReg > vertex)
    {
      # m11 <- integrate(f111, lower = minReg, upper = vertex)$value + integrate(f112, lower = vertex, upper = maxReg)$value
      m11 <- 1
      m12 <- integrate(f121, lower = minReg, upper = vertex)$value + integrate(f122, lower = vertex, upper = maxReg)$value
      m13 <- integrate(f131, lower = minReg, upper = vertex)$value + integrate(f132, lower = vertex, upper = maxReg)$value
      m21 <- m12
      m22 <- integrate(f221, lower = minReg, upper = vertex)$value + integrate(f222, lower = vertex, upper = maxReg)$value
      m23 <- integrate(f231, lower = minReg, upper = vertex)$value + integrate(f232, lower = vertex, upper = maxReg)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f331, lower = minReg, upper = vertex)$value + integrate(f332, lower = vertex, upper = maxReg)$value
    }
    else if(near(vertex, maxReg))
    {
      # m11 <- integrate(f111, lower = minReg, upper = vertex)$value
      m11 <- 1
      m12 <- integrate(f121, lower = minReg, upper = vertex)$value
      m13 <- integrate(f131, lower = minReg, upper = vertex)$value
      m21 <- m12
      m22 <- integrate(f221, lower = minReg, upper = vertex)$value
      m23 <- integrate(f231, lower = minReg, upper = vertex)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f331, lower = minReg, upper = vertex)$value
    }
    else
    {
      m11 <- 1
      # m11 <- integrate(f112, lower = vertex, upper = maxReg)$value
      m12 <- integrate(f122, lower = vertex, upper = maxReg)$value
      m13 <- integrate(f132, lower = vertex, upper = maxReg)$value
      m21 <- m12
      m22 <- integrate(f222, lower = vertex, upper = maxReg)$value
      m23 <- integrate(f232, lower = vertex, upper = maxReg)$value
      m31 <- m13
      m32 <- m23
      m33 <- integrate(f332, lower = vertex, upper = maxReg)$value
    }

  }



  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}


# IntMatAntoineHet(8.07131, 1730.63, 233.426, 80, 100, 0, option = "Unif")

# IntMatAntoine(8.07131, 1730.63, 233.426, 98, 102, 100, option = "Triang")

# library(tidyverse)
# IntMatAntoine(8.07131, 1730.63, 233.426, 98, 100, 100, option = "Triang")


# Función de sensibilidad para I-Optimalidad de la ecuación de Antoine
IsensAntoineHet <- function(a, b, c, mat, matB = diag(3)) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  sensMat <- solve(mat) %*% matB %*% solve(mat)
  sensX <- function(x) {
    return(sensMat[[1,1]] + sensMat[[3,1]]*b/(c + x)^2 - (sensMat[[2,1]]/(c + x)) -
             (sensMat[[1,2]] + sensMat[[3,2]]*b/(c + x)^2 - sensMat[[2,2]]/(c + x))/(c + x) +
             b*(sensMat[[1,3]] + sensMat[[3,3]]*b/(c + x)^2 - sensMat[[2,3]]/(c + x))/(c + x)^2)
  }
  return(sensX)
}

# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
#
# desMat <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, optdes)
#
# sens <- IsensAntoineHet(8.07131, 1730.63, 233.426, desMat)
#
# sens(1)/10^8

# B <- IntMatAntoine(8.07131, 1730.63, 233.426, 80, 120, 0, option = "Unif")
#
# optdes <- data.frame(Point = c(100, 83.9316, 33.2345), Weight = c(0.359629, 0.377516, 0.262233))
#
# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)
#
# sens <- IsensAntoine(8.07131, 1730.63, 233.426, desMat, B)
#
# sens(70)
#
# crit <- icrit(desMat, B)
#
# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) crit, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")



IWFMultHet <- function(A, B, C, initDes, matB = diag(3), min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
  critVal <- numeric(2122)
  index <- 1
  for(i in 1:21) {
    M <- dmatrixAntoineHet(A, B, C, initDes)
    critVal[index] <- icrit(M, matB)
    index <- index + 1
    sens <- IsensAntoineHet(A, B, C, M, matB)
    xmax <- findmax(sens, min, max, grid.length)
    if (((sens(xmax) - critVal[index-1])/critVal[index-1]) < tol) {
      break
    }
    initDes <- updateDesign(initDes, xmax, 1/(i+3), joinThresh)
    iter <- 1
    maxiter <- 100
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- dmatrixAntoineHet(A, B, C, initDes)
      crit <- icrit(M, matB)
      critVal[index] <- crit
      index <- index + 1
      sens <- IsensAntoineHet(A, B, C, M, matB)
      initDes$Weight <- updateWeightsI(initDes, sens, crit, deltaW)
      initDes$Weight <- initDes$Weight/sum(initDes$Weight)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i%%5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  M <- dmatrixAntoineHet(A, B, C, initDes)
  critVal[index] <- icrit(M, matB)
  critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  return(list("optdes" = initDes, "convergence" = conv))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# matUnif <- IntMatAntoineHet(8.07131, 1730.63, 233.426, 80, 100, 100, option = "Unif")
# matTriang <- IntMatAntoineHet(8.07131, 1730.63, 233.426, 80, 100, 100, option = "Triang")
# optdes <- IWFMultHet(8.07131, 1730.63, 233.426, des, matUnif, 1, 100, 1000, 9, 0.01, 1/2, 0.00001)
# optdes1 <- IWFMultHet(8.07131, 1730.63, 233.426, des, matTriang, 1, 100, 1000, 9, 0.01, 1/2, 0.00001)

# optdes <- IWFMultHet(8.07131, 1730.63, 233.426, des, diag(3), 1, 100, 1000, 9, 0.01, 1/2, 0.00001)
# system.time(for(i in 1:100) {IWFMultHet(8.07131, 1730.63, 233.426, des, diag(3), 1, 100, 1000, 9, 0.01, 1/2, 0.00001)})
# optdes$optdes
# optdes1$optdes
# M <- dmatrixAntoineHet(8.07131, 1730.63, 233.426, optdes1$optdes)
# sens <- IsensAntoineHet(8.07131, 1730.63, 233.426, M, matTriang)
# crit <- icrit(M, matTriang)


# p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
# p + stat_function(fun = sens) + stat_function(fun = function(x) crit, col = "blue") + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")

# optdes2 <- des <- data.frame(Point = c(1, 65.8262, 100), Weight = c(1/3, 1/3, 1/3))



####  Funciones generales
eff <- function(Criterion, mat1, mat2, k = 3, s = 2, intPars = 2, matB = diag(3)){
  if(identical(Criterion, "D-Optimality")){
    return(deff(mat1, mat2, k))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(dseff(mat1, mat2, s, intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(ieff(mat1, mat2, matB))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(ieff(mat1, mat2, matB))
  }
}

sens <- function(Variance, Criterion, a, b, c, mat, intPars = 0, matB = diag(3)){
  if(identical(Variance, "Homoscedastic"))
  {
        if(identical(Criterion, "D-Optimality")){
          return(dsensAntoine(a, b, c, mat))
        }
        else if(identical(Criterion, "Ds-Optimality")){
          return(DSsensAntoine(a, b, c, mat, intPars))
        }
        else if(identical(Criterion, "A-Optimality")){
          return(IsensAntoine(a, b, c, mat, matB))
        }
        else if(identical(Criterion, "I-Optimality")){
          return(IsensAntoine(a, b, c, mat, matB))
        }
  } else if(identical(Variance, "Heteroscedastic"))
  {
    if(identical(Criterion, "D-Optimality")){
      return(dsensAntoineHet(a, b, c, mat))
    }
    else if(identical(Criterion, "Ds-Optimality")){
      return(DSsensAntoineHet(a, b, c, mat, intPars))
    }
    else if(identical(Criterion, "A-Optimality")){
      return(IsensAntoineHet(a, b, c, mat, matB))
    }
    else if(identical(Criterion, "I-Optimality")){
      return(IsensAntoineHet(a, b, c, mat, matB))
    }
  }
}

crit <- function(Criterion, mat, k = 3, s = 2, intPars = 2, matB = diag(3)){
  if(identical(Criterion, "D-Optimality")){
    return(dcrit(mat, k))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(dscrit(mat, s, intPars))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(icrit(mat, matB))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(icrit(mat, matB))
  }
}


WF <- function(Variance, Criterion, A, B, C, initDes, intPar = 0, matB = diag(3), min, max, grid.length, joinThresh, deleteThresh, k = 3, s = 2, deltaW, tol){
  if(identical(Variance, "Homoscedastic")){
    if(identical(Criterion, "D-Optimality")){
      return(WFMult(A, B, C, initDes, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol))
    }
    else if(identical(Criterion, "Ds-Optimality")){
      return(DSWFMult(A, B, C, initDes, intPar, min, max, grid.length, joinThresh, deleteThresh, s, deltaW, tol))
    }
    else if(identical(Criterion, "A-Optimality")){
      return(IWFMult(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
    }
    else if(identical(Criterion, "I-Optimality")){
      return(IWFMult(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
    }
  } else if(identical(Variance, "Heteroscedastic")) {
    if(identical(Criterion, "D-Optimality")){
      return(WFMultHet(A, B, C, initDes, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol))
    }
    else if(identical(Criterion, "Ds-Optimality")){
      return(DSWFMultHet(A, B, C, initDes, intPar, min, max, grid.length, joinThresh, deleteThresh, s, deltaW, tol))
    }
    else if(identical(Criterion, "A-Optimality")){
      return(IWFMultHet(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
    }
    else if(identical(Criterion, "I-Optimality")){
      return(IWFMultHet(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
    }
  }
}

dmatrixAntoine <- function(Variance, a, b, c, design) {
  if(identical(Variance, "Homoscedastic")){
    return(dmatrixAntoineHom(a, b, c, design))
  } else if(identical(Variance, "Heteroscedastic")){
    return(dmatrixAntoineHet(a, b, c, design))
  }
}

doptAntoine <- function(Variance, A, B, C, min, max){
  if(identical(Variance, "Homoscedastic")){
    return(doptAntoineHom(A, B, C, min, max))
  } else if(identical(Variance, "Heteroscedastic")){
    return(doptAntoineHet(A, B, C, min, max))
  }
}
# doptAntoine("Heteroscedastic", 8.07131, 1730.63, 233.426, 1, 100)

IntMatAntoine <- function(Variance, a, b, c, minReg, maxReg, vertex, option = "Unif") {
  if(identical(Variance, "Homoscedastic")){
    return(IntMatAntoineHom(a, b, c, minReg, maxReg, vertex, option))
  } else if(identical(Variance, "Heteroscedastic")){
    return(IntMatAntoineHet(a, b, c, minReg, maxReg, vertex, option))
  }
}

# eff <- function(Criterion, mat1, mat2, k = 3, s = 2, intPars = 2, matB = diag(3)){
#   if(identical(Criterion, "D-Optimality")){
#     return(deff(mat1, mat2, k))
#   }
#   else if(identical(Criterion, "Ds-Optimality")){
#     return(dseff(mat1, mat2, s, intPars))
#   }
#   else if(identical(Criterion, "A-Optimality")){
#     return(ieff(mat1, mat2, matB))
#   }
#   else if(identical(Criterion, "I-Optimality")){
#     return(ieff(mat1, mat2, matB))
#   }
# }
#
# sens <- function(Criterion, a, b, c, mat, intPars = 0, matB = diag(3)){
#   if(identical(Criterion, "D-Optimality")){
#     return(dsensAntoine(a, b, c, mat))
#   }
#   else if(identical(Criterion, "Ds-Optimality")){
#     return(DSsensAntoine(a, b, c, mat, intPars))
#   }
#   else if(identical(Criterion, "A-Optimality")){
#     return(IsensAntoine(a, b, c, mat, matB))
#   }
#   else if(identical(Criterion, "I-Optimality")){
#     return(IsensAntoine(a, b, c, mat, matB))
#   }
# }
#
# crit <- function(Criterion, mat, k = 3, s = 2, intPars = 2, matB = diag(3)){
#   if(identical(Criterion, "D-Optimality")){
#     return(dcrit(mat, k))
#   }
#   else if(identical(Criterion, "Ds-Optimality")){
#     return(dscrit(mat, s, intPars))
#   }
#   else if(identical(Criterion, "A-Optimality")){
#     return(icrit(mat, matB))
#   }
#   else if(identical(Criterion, "I-Optimality")){
#     return(icrit(mat, matB))
#   }
# }
#
#
# WF <- function(Criterion, A, B, C, initDes, intPar = 0, matB = diag(3), min, max, grid.length, joinThresh, deleteThresh, k = 3, s = 2, deltaW, tol){
#   if(identical(Criterion, "D-Optimality")){
#     return(WFMult(A, B, C, initDes, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol))
#   }
#   else if(identical(Criterion, "Ds-Optimality")){
#     return(DSWFMult(A, B, C, initDes, intPar, min, max, grid.length, joinThresh, deleteThresh, s, deltaW, tol))
#   }
#   else if(identical(Criterion, "A-Optimality")){
#     return(IWFMult(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
#   }
#   else if(identical(Criterion, "I-Optimality")){
#     return(IWFMult(A, B, C, initDes, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
#   }
# }
