# Algoritmo de cálculo
# Requiere, además de lo "habitual" la función de sensibilidad, la matriz de información y el criterio
# Llama, en función de crit, al algoritmo de WF pertinente
# A futuro: admitir también matriz de la región de interés/parámetros de interés
# POSIBLE: ofrecer outputs -> diseño, plot y convergencia, con un vector? Sin convergencia y un parámetro para el plot?
WFMult <- function(initDes, grad, Criterion, intPars = NA, matB = NA, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol){
  if(identical(Criterion, "D-Optimality")){
    return(DWFMult(initDes, grad, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol))
  }
  else if(identical(Criterion, "Ds-Optimality")){
    return(DsWFMult(initDes, grad, intPars, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
  }
  else if(identical(Criterion, "A-Optimality")){
    return(IWFMult(initDes, grad, diag(k),min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
  }
  else if(identical(Criterion, "I-Optimality")){
    return(IWFMult(initDes, grad, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol))
  }
}

# Algoritmo WF multiplicativo para D-opt
DWFMult <- function(initDes, grad, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol) {
  # critVal <- numeric(2122)
  # index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for(i in 1:21) {
    M <- inf_mat(grad, initDes)
    # critVal[index] <- dcrit(M, k)
    # index <- index + 1
    sensM <- dsens(grad, M)
    xmax <- findmax(sensM, min, max, grid.length)
    initDes <- updateDesign(initDes, xmax, joinThresh)
    iter <- 1
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- inf_mat(grad, initDes)
      # critVal[index] <- dcrit(M, k)
      # index <- index + 1
      sensM <- dsens(grad, M)
      initDes$Weight <- updateWeights(initDes, sensM, k, deltaW)
      stopw <- any(max(abs(weightsInit - initDes$Weight) < tol)) || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
  }
  # critVal[index] <- dcrit(M, k)
  # critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  # conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  plot_opt <- plot_sens(min, max, sensM, k)
  list("optdes" = initDes#, "convergence" = conv
       ,"sens" = plot_opt)
}

# Algoritmo WF multiplicativo para Ds-opt
DsWFMult <- function(initDes, grad, intPar, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
  # critVal <- numeric(2122)
  # index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for(i in 1:21) {
    M <- inf_mat(grad, initDes)
    # critVal[index] <- dscrit(M, s, intPar)
    # index <- index + 1
    sensDs <- dssens(grad, M, intPar)
    xmax <- findmax(sensDs, min, max, grid.length)
    initDes <- updateDesign(initDes, xmax, joinThresh)
    iter <- 1
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      # critVal[index] <- dscrit(M, s, intPar)
      # index <- index + 1
      M <- inf_mat(grad, initDes)
      sensDs <- dssens(grad, M, intPar)
      initDes$Weight <- updateWeightsDS(initDes, sensDs, length(intPar), deltaW)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i %% 5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  # critVal[index] <- dscrit(M, s, intPar)
  # critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  # conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  plot_opt <-plot_sens(min, max, sensDs, length(intPar))
  list("optdes" = initDes#, "convergence" = conv
       ,"sens" = plot_opt)
}


# Algoritmo WF multiplicativo para I-opt (A-opt con B=diag(n))
IWFMult <- function(initDes, grad, matB = diag(3), min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
  # critVal <- numeric(2122)
  # index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for(i in 1:21) {
    M <- inf_mat(grad, initDes)
    # critVal[index] <- icrit(M, matB)
    # index <- index + 1
    sensI <- isens(grad, M, matB)
    xmax <- findmax(sensI, min, max, grid.length)
    initDes <- updateDesign(initDes, xmax, joinThresh)
    iter <- 1
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      M <- inf_mat(grad, initDes)
      crit <- icrit(M, matB)
      # critVal[index] <- crit
      # index <- index + 1
      sensI <- isens(grad, M, matB)
      initDes$Weight <- updateWeightsI(initDes, sensI, crit, deltaW)
      stopw <- max(abs(weightsInit - initDes$Weight)) < tol || iter >= maxiter
      iter <- iter + 1
    }
    initDes <- deletePoints(initDes, deleteThresh)
    if(i %% 5 == 0)
      initDes <- updateDesignTotal(initDes, 8)
  }
  # M <- dmatrixAntoine(A, B, C, initDes)
  # critVal[index] <- icrit(M, matB)
  # critVal <- critVal[1:(length(critVal)-sum(critVal == 0))]
  # conv <- data.frame("criteria" = critVal, "step" = seq(1, length(critVal), 1))
  plot_opt <-plot_sens(min, max, sensI, icrit(M, matB))
  list("optdes" = initDes#, "convergence" = conv
       ,"sens" = plot_opt)
}


# Función accesible para el usuario que llama a las necesarias para calcular el diseño óptimo según las
# opciones especificadas


#' Calculates the optimal design for a specified Criterion
#'
#' @description
#' The opt_des function calculates the optimal design for an optimality Criterion and a model input from the user.
#' The parameters allows for the user to customize the parameters for the cocktail algorithm in case the default
#' set don't provide a satisfactory output. Depending on the criterion
#'
#' @param Criterion
#' @param model
#' @param parameters
#' @param par_values
#' @param design_space
#' @param init_design
#' @param joinThresh
#' @param deleteThresh
#' @param delta
#' @param tol
#' @param matB
#' @param par_int
#' @param reg_int
#' @param desired_output
#'
#' @return
#' @export
#'
#' @examples
opt_des <- function(Criterion, model, parameters, par_values, design_space,
                    init_design = NA,
                    joinThresh = -1,
                    deleteThresh = 0.02,
                    delta = 1/2,
                    tol = 0.00001,
                    matB = NA,
                    par_int = NA,
                    reg_int = NA,
                    desired_output = c(1, 2)
){
  # check_inputs()
  k <- length(par_values)
  if(is.na(init_design)) init_design <- data.frame("Point" = seq(design_space[[1]], design_space[[2]],length.out = k), "Weight" = rep(1/k, times = k))
  grad <- gradient(model, parameters, par_values)
  if(joinThresh == -1) joinThresh <- (design_space[[2]] - design_space[[1]])/10
  output <- WFMult(init_design, grad, Criterion, intPars = par_int, matB, design_space[[1]], design_space[[2]], 1000, joinThresh, deleteThresh, k, delta, tol)
}


# library(devtools)
#
# install_github("kezrael/optedr")
#
# library(optedr)
#
# result <- opt_des("D-Optimality", y ~ a*exp(-b/x), c("a", "b"), c(1, 1500), c(212, 422))
#
# result$optdes
#
# result$sens
#
#
# result1 <- opt_des("A-Optimality", y ~ a*exp(-b/x), c("a", "b"), c(1, 1500), c(212, 422))
#
# result1$optdes
#
# result1$sens
#
#
# result2 <- opt_des("Ds-Optimality", y ~ a*exp(-b/x), c("a", "b"), c(1, 1500), c(212, 422), par_int = c(1))
#
# result2$optdes
#
# result2$sens


