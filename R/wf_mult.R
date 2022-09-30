#' Master function for the cocktail algorithm, that calls the appropriate one given the criterion.
#'
#' @description
#' Depending on the \code{Criterion} the cocktail algorithm for the chosen criterion is called,
#' and the necessary parameters for the functions are given from the user input.
#'
#' @param init_design with the initial design for the algorithm. A dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param grad function of partial derivatives of the model.
#' @param Criterion character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param par_int numeric vector with the index of the \code{parameters} of interest. Only necessary when
#'   the \code{Criterion} chosen is 'Ds-Optimality'.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region.
#' @param min numeric value with the inferior bound of the space of the design.
#' @param max numeric value with the upper bound of the space of the design.
#' @param grid.length numeric value that gives the grid to evaluate the sensitivity function when looking for a
#'   maximum.
#' @param join_thresh numeric value that states how close, in real units, two points must be in order to
#'   be joined together by the join heuristic.
#' @param delete_thresh numeric value with the minimum weight, over 1 total, that a point needs to have
#'   in order to not be deleted from the design.
#' @param k number of unknown parameters of the model.
#' @param delta_weights numeric value in (0, 1), parameter of the algorithm.
#' @param tol numeric value for the convergence of the weight optimizing algorithm.
#' @param tol2 numeric value for the stop condition of the algorithm.
#'
#' @return list correspondent to the output of the correspondent algorithm called, dependent on the criterion.
#'   A list of two objects:
#'   * optdes: a dataframe with the optimal design in two columns, \code{Point} and \code{Weight}.
#'   * sens: a plot with the sensitivity function to check for optimality of the design.
#'
#' @family cocktail algorithms
WFMult <- function(init_design, grad, Criterion, par_int = NA, matB = NA, min, max, grid.length, join_thresh, delete_thresh, k, delta_weights, tol, tol2) {
  if (identical(Criterion, "D-Optimality")) {
    return(DWFMult(init_design, grad, min, max, grid.length, join_thresh, delete_thresh, k, delta_weights, tol, tol2))
  }
  else if (identical(Criterion, "Ds-Optimality")) {
    return(DsWFMult(init_design, grad, par_int, min, max, grid.length, join_thresh, delete_thresh, delta_weights, tol, tol2))
  }
  else if (identical(Criterion, "A-Optimality")) {
    return(IWFMult(init_design, grad, diag(k), min, max, grid.length, join_thresh, delete_thresh, delta_weights, tol, tol2))
  }
  else if (identical(Criterion, "I-Optimality")) {
    return(IWFMult(init_design, grad, matB, min, max, grid.length, join_thresh, delete_thresh, delta_weights, tol, tol2))
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
DWFMult <- function(init_design, grad, min, max, grid.length, join_thresh, delete_thresh, k, delta_weights, tol, tol2) {
  Point <- NULL
  crit_val <- numeric(2122)
  index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for (i in 1:21) {
    M <- inf_mat(grad, init_design)
    crit_val[index] <- dcrit(M, k)
    index <- index + 1
    sensM <- dsens(grad, M)
    xmax <- findmax(sensM, min, max, grid.length)
    if ((sensM(xmax) - k) / k < tol2) {
      message(crayon::blue(cli::symbol$info), " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1
    stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      M <- inf_mat(grad, init_design)
      crit_val[index] <- dcrit(M, k)
      index <- index + 1
      sensM <- dsens(grad, M)
      init_design$Weight <- update_weights(init_design, sensM, k, delta_weights)
      stopw <- any(max(abs(weightsInit - init_design$Weight) < tol)) || iter >= maxiter
      iter <- iter + 1
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) {
      init_design <- update_design_total(init_design, join_thresh)
    }
    if (i == 21) {
      message(crayon::blue(cli::symbol$info), " Stop condition not reached, max iterations performed")
    }
  }
  crit_val[index] <- dcrit(M, k)
  crit_val <- crit_val[1:(length(crit_val) - sum(crit_val == 0))]
  conv <- data.frame("criteria" = crit_val, "step" = seq(1, length(crit_val), 1))
  conv_plot <- plot_convergence(conv)

  init_design <- dplyr::arrange(init_design, Point)
  rownames(init_design) <- NULL

  M <- inf_mat(grad, init_design)
  sensM <- dsens(grad, M)
  xmax <- findmax(sensM, min, max, grid.length * 10)

  atwood <- k / sensM(xmax) * 100

  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- plot_sens(min, max, sensM, k)
  l_return <- list(
    "optdes" = init_design, "convergence" = conv_plot,
    "sens" = plot_opt, "criterion" = "D-Optimality", "crit_value" = crit_val[length(crit_val)]
  )
  attr(l_return, "hidden_value") <- k
  attr(l_return, "gradient") <- grad
  attr(l_return, "atwood") <- atwood
  class(l_return) <- "optdes"
  l_return
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
DsWFMult <- function(init_design, grad, par_int, min, max, grid.length, join_thresh, delete_thresh, delta_weights, tol, tol2) {
  Point <- NULL
  crit_val <- numeric(2122)
  index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for (i in 1:21) {
    M <- inf_mat(grad, init_design)
    crit_val[index] <- dscrit(M, par_int)
    index <- index + 1
    sensDs <- dssens(grad, M, par_int)
    xmax <- findmax(sensDs, min, max, grid.length)
    if ((sensDs(xmax) - length(par_int)) / length(par_int) < tol2) {
      message(crayon::blue(cli::symbol$info), " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1
    stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      crit_val[index] <- dscrit(M, par_int)
      index <- index + 1
      M <- inf_mat(grad, init_design)
      sensDs <- dssens(grad, M, par_int)
      init_design$Weight <- update_weightsDS(init_design, sensDs, length(par_int), delta_weights)
      stopw <- any(max(abs(weightsInit - init_design$Weight)) < tol) || iter >= maxiter
      iter <- iter + 1
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) {
      init_design <- update_design_total(init_design, join_thresh)
    }
    if (i == 21) {
      message(crayon::blue(cli::symbol$info), " Stop condition not reached, max iterations performed")
    }
  }
  crit_val[index] <- dscrit(M, par_int)
  crit_val <- crit_val[1:(length(crit_val) - sum(crit_val == 0))]
  conv <- data.frame("criteria" = crit_val, "step" = seq(1, length(crit_val), 1))
  conv_plot <- plot_convergence(conv)

  init_design <- dplyr::arrange(init_design, Point)
  rownames(init_design) <- NULL

  M <- inf_mat(grad, init_design)
  sensM <- dssens(grad, M, par_int)
  xmax <- findmax(sensM, min, max, grid.length * 10)

  atwood <- (2 - sensM(xmax) / length(par_int)) * 100

  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")


  plot_opt <- plot_sens(min, max, sensDs, length(par_int))
  l_return <- list(
    "optdes" = init_design, "convergence" = conv_plot,
    "sens" = plot_opt, "criterion" = "Ds-Optimality", "crit_value" = crit_val[length(crit_val)]
  )
  attr(l_return, "hidden_value") <- par_int
  attr(l_return, "gradient") <- grad
  attr(l_return, "atwood") <- atwood
  class(l_return) <- "optdes"
  l_return
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
IWFMult <- function(init_design, grad, matB, min, max, grid.length, join_thresh, delete_thresh, delta_weights, tol, tol2) {
  Point <- NULL
  crit_val <- numeric(2122)
  index <- 1
  # Maximum iterations for the optimize weights loop
  maxiter <- 100
  for (i in 1:21) {
    M <- inf_mat(grad, init_design)
    crit_val[index] <- icrit(M, matB)
    index <- index + 1
    sensI <- isens(grad, M, matB)
    xmax <- findmax(sensI, min, max, grid.length)
    if ((sensI(xmax) - crit_val[index-1]) < tol2) {
      message(crayon::blue(cli::symbol$info), " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1
    stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      M <- inf_mat(grad, init_design)
      crit <- icrit(M, matB)
      crit_val[index] <- crit
      index <- index + 1
      sensI <- isens(grad, M, matB)
      init_design$Weight <- update_weightsI(init_design, sensI, crit, delta_weights)
      stopw <- any(max(abs(weightsInit - init_design$Weight)) < tol) || iter >= maxiter
      iter <- iter + 1
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) {
      init_design <- update_design_total(init_design, join_thresh)
    }
    if (i == 21) {
      message(crayon::blue(cli::symbol$info), " Stop condition not reached, max iterations performed")
    }
  }
  M <- inf_mat(grad, init_design)
  crit_val[index] <- icrit(M, matB)
  crit_val <- crit_val[1:(length(crit_val) - sum(crit_val == 0))]
  conv <- data.frame("criteria" = crit_val, "step" = seq(1, length(crit_val), 1))
  conv_plot <- plot_convergence(conv)

  init_design <- dplyr::arrange(init_design, Point)
  rownames(init_design) <- NULL


  M <- inf_mat(grad, init_design)
  sensM <- isens(grad, M, matB)
  xmax <- findmax(sensM, min, max, grid.length * 10)

  atwood <- (2 - sensM(xmax) / icrit(M, matB)) * 100

  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")


  plot_opt <- plot_sens(min, max, sensI, icrit(M, matB))
  l_return <- list(
    "optdes" = init_design, "convergence" = conv_plot,
    "sens" = plot_opt, "criterion" = "I-Optimality", "crit_value" = crit_val[length(crit_val)]
  )
  attr(l_return, "hidden_value") <- matB
  attr(l_return, "gradient") <- grad
  attr(l_return, "atwood") <- atwood
  class(l_return) <- "optdes"
  l_return
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
#' @param join_thresh optional numeric value that states how close, in real units, two points must be in order to
#'   be joined together by the join heuristic.
#' @param delete_thresh optional numeric value with the minimum weight, over 1 total, that a point needs to have
#'   in order to not be deleted from the design.
#' @param delta optional numeric value in (0, 1), parameter of the algorithm.
#' @param tol optional numeric value for the convergence of the weight optimizing algorithm.
#' @param tol2 optional numeric value for the stop criterion: difference between maximum of sensitivity function
#'   and optimality criterion.
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region.
#' @param reg_int optional numeric vector of two components with the bounds of the interest region for I-Optimality.
#' @param desired_output not functional yet: decide which kind of output you want.
#' @param distribution character specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal'
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#'
#' @return a list of two objects:
#'   * optdes: a dataframe with the optimal design in two columns, \code{Point} and \code{Weight}.
#'   * sens: a plot with the sensitivity function to check for optimality of the design.
#' @export
#'
#' @examples
#' opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
opt_des <- function(Criterion, model, parameters,
                    par_values = c(1),
                    design_space,
                    init_design = NULL,
                    join_thresh = -1,
                    delete_thresh = 0.02,
                    delta = 1 / 2,
                    tol = 0.00001,
                    tol2 = 0.00001,
                    par_int = NULL,
                    matB = NULL,
                    reg_int = NULL,
                    desired_output = c(1, 2),
                    distribution = NA,
                    weight_fun = function(x) 1) {
  k <- length(parameters)
  if(identical(par_values, c(1))){
    par_values <- rep(1, length(parameters))
  }
  if (is.null(init_design)) init_design <- data.frame("Point" = seq(design_space[[1]], design_space[[2]], length.out = k * (k + 1) / 2 + 1), "Weight" = rep(1 / (k * (k + 1) / 2 + 1), times = k * (k + 1) / 2 + 1))
  check_inputs(
    Criterion, model, parameters, par_values, design_space, init_design, join_thresh, delete_thresh,
    delta, tol, tol2, par_int, matB, reg_int, desired_output, weight_fun
  )
  if (design_space[1] > design_space[2]) design_space <- rev(design_space)
  grad <- gradient(model, parameters, par_values, weight_fun)
  if (join_thresh == -1) join_thresh <- (design_space[[2]] - design_space[[1]]) / 10
  if (identical(Criterion, "I-Optimality") && is.null(matB)) {
    if (!is.null(reg_int)) {
      matB <- integrate_reg_int(grad, k, reg_int)
    }
  }

  if(!is.na(distribution)){
    weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
  }

  output <- WFMult(init_design, grad, Criterion, par_int = par_int, matB, design_space[[1]], design_space[[2]], 1000, join_thresh, delete_thresh, k, delta, tol, tol2)
  attr(output, "model") <- model
  attr(output, "weight_fun") <- weight_fun
  return(output)
}


