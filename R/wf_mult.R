# Algoritmo de cálculo
# Requiere, además de lo "habitual" la función de sensibilidad, la matriz de información y el criterio
# Llama, en función de crit, al algoritmo de WF pertinente
# A futuro: admitir también matriz de la región de interés/parámetros de interés
# POSIBLE: ofrecer outputs -> diseño, plot y convergencia, con un vector? Sin convergencia y un parámetro para el plot?

#' Master function for the cocktail algorithm, that calls the appropriate one given the criterion.
#'
#' @description
#' Depending on the \code{Criterion} the cocktail algorithm for the chosen criterion is called,
#' and the necessary parameters for the functions are given from the user input.
#'
#' @param initDes with the initial design for the algorithm. A dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param grad function of partial derivatives of the model.
#' @param Criterion character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param intPars numeric vector with the index of the \code{parameters} of interest. Only necessary when
#'   the \code{Criterion} chosen is 'Ds-Optimality'.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region.
#' @param min numeric value with the inferior bound of the space of the design.
#' @param max numeric value with the upper bound of the space of the design.
#' @param grid.length numeric value that gives the grid to evaluate the sensitivity function when looking for a
#'   maximum.
#' @param joinThresh numeric value that states how close, in real units, two points must be in order to
#'   be joined together by the join heuristic.
#' @param deleteThresh numeric value with the minimum weight, over 1 total, that a point needs to have
#'   in order to not be deleted from the design.
#' @param k number of unknown parameters of the model.
#' @param deltaW numeric value in (0, 1), parameter of the algorithm.
#' @param tol numeric value for the convergence of the weight optimizing algorithm.
#'
#' @return list correspondent to the output of the correspondent algorithm called, dependent on the criterion.
#'   A list of two objects:
#'   * optdes: a dataframe with the optimal design in two columns, \code{Point} and \code{Weight}.
#'   * sens: a plot with the sensitivity function to check for optimality of the design.
#'
#' @family cocktail algorithms
#'
#' @examples
#' \dontrun{
#'   WFMult(initDes, grad, Criterion, intPars = NA, matB = NA, min, max, grid.length, joinThresh, deleteThresh, k, deltaW, tol)
#' }
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


#' Cocktail Algorithm implementation for D-Optimality
#'
#' @description
#' Function that calculates the DsOptimal design. The rest of the parameters can help the convergence of the
#' algorithm.
#'
#' @inherit WFMult return params examples
#'
#' @family cocktail algorithms
#'
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

#' Cocktail Algorithm implementation for Ds-Optimality
#'
#' @description
#' Function that calculates the Ds-Optimal designs for the interest parameters given by \code{intPar}. The
#' rest of the parameters can help the convergence of the algorithm.
#'
#' @inherit WFMult return params examples
#'
#' @family cocktail algorithms
#'
DsWFMult <- function(initDes, grad, intPars, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
  # critVal <- numeric(2122)
  # index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for(i in 1:21) {
    M <- inf_mat(grad, initDes)
    # critVal[index] <- dscrit(M, s, intPar)
    # index <- index + 1
    sensDs <- dssens(grad, M, intPars)
    xmax <- findmax(sensDs, min, max, grid.length)
    initDes <- updateDesign(initDes, xmax, joinThresh)
    iter <- 1
    stopw <- FALSE
    while(!stopw) {
      weightsInit <- initDes$Weight
      # critVal[index] <- dscrit(M, s, intPar)
      # index <- index + 1
      M <- inf_mat(grad, initDes)
      sensDs <- dssens(grad, M, intPars)
      initDes$Weight <- updateWeightsDS(initDes, sensDs, length(intPars), deltaW)
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
  plot_opt <-plot_sens(min, max, sensDs, length(intPars))
  list("optdes" = initDes#, "convergence" = conv
       ,"sens" = plot_opt)
}


#' Cocktail Algorithm implementation for I-Optimality and A-Optimality (with matB = diag(k))
#'
#' @description
#' Function that calculates the I-Optimal designs given the matrix B (should be integral of the information matrix
#' over the interest region), or A-Optimal if given diag(k). The rest of the parameters can help the convergence
#' of the algorithm.
#'
#' @inherit WFMult return params examples
#'
#' @family cocktail algorithms
#'
IWFMult <- function(initDes, grad, matB, min, max, grid.length, joinThresh, deleteThresh, deltaW, tol) {
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



#' Calculates the optimal design for a specified Criterion
#'
#' @description
#' The opt_des function calculates the optimal design for an optimality Criterion and a model input from the user.
#' The parameters allows for the user to customize the parameters for the cocktail algorithm in case the default
#' set don't provide a satisfactory output. Depending on the criterion, additional details are necessary.
#' For 'Ds-Optimality' the par_int parameter is necessary. For 'I-Optimality' either the matB or reg_int must
#' be provided.
#'
#' @param Criterion character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param model formula describing the model to calculate the optimal design. Must use x as the variable.
#' @param parameters character vector with the parameters of the models, as written in the \code{formula}.
#' @param par_values numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.
#' @param design_space numeric vector of length 2, first component with the minimum of the space of the design and
#'   second component with the maximum.
#' @param init_design optimal dataframe with the initial design for the algorithm. A dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param joinThresh optional numeric value that states how close, in real units, two points must be in order to
#'   be joined together by the join heuristic.
#' @param deleteThresh optional numeric value with the minimum weight, over 1 total, that a point needs to have
#'   in order to not be deleted from the design.
#' @param delta optional numeric value in (0, 1), parameter of the algorithm.
#' @param tol optional numeric value for the convergence of the weight optimizing algorithm.
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region.
#' @param reg_int optional numeric vector of two components with the bounds of the interest region for I-Optimality.
#' @param desired_output not functional yet: decide which kind of output you want.
#'
#' @return a list of two objects:
#'   * optdes: a dataframe with the optimal design in two columns, \code{Point} and \code{Weight}.
#'   * sens: a plot with the sensitivity function to check for optimality of the design.
#' @export
#'
#' @examples
#' opt_des("D-Optimality", y ~ a*exp(-b/x), c("a", "b"), c(1, 1500), c(212, 422))
opt_des <- function(Criterion, model, parameters, par_values, design_space,
                    init_design = NA,
                    joinThresh = -1,
                    deleteThresh = 0.02,
                    delta = 1/2,
                    tol = 0.00001,
                    par_int = NA,
                    matB = NA,
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


