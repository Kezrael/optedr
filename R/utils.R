# Auxiliar function for algorithms --------------------------------------



#' Weight function per distribution
#'
#' @param model formula describing the model to use. Must use x as the variable.
#' @param char_vars character vector with the parameters of the models, as written in the \code{formula}
#' @param values numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.
#' @param distribution character variable specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal' (work in progress)
#'
#' @return one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response.
#'
weight_function <- function(model, char_vars, values, distribution = "Homoscedasticity") {
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  if(!(distribution %in% c("Poisson", "Gamma", "Logistic", #"log-normal",
                           "Homoscedasticity"))){
    warning(crayon::yellow(cli::symbol$warning), " Not a valid distribution specified, using a normal homoscedastic")
    return(function(x) 1)
  }
  else if(distribution == "Homoscedasticity"){
    return(function(x) 1)
  }
  else if(distribution == "Poisson"){
    cmd <- utils::tail(as.character(model),1)
    expres <- parse(text=cmd)
    lista <- values
    names(lista) <- char_vars
    f <- function(x_val) {
      exp(eval(expres, c(lista, list("x" = x_val)))/2)
    }
  }
  else if(distribution == "Gamma"){
    cmd <- utils::tail(as.character(model),1)
    expres <- parse(text=cmd)
    lista <- values
    names(lista) <- char_vars
    f <- function(x_val) {
      (eval(expres, c(lista, list("x" = x_val))))^(-1)
    }
  }
  else if(distribution == "Logistic"){
    cmd <- utils::tail(as.character(model),1)
    expres <- parse(text=cmd)
    lista <- values
    names(lista) <- char_vars
    f <- function(x_val) {
      sqrt(exp(-eval(expres, c(lista, list("x" = x_val))))/(1 + exp(-eval(expres, c(lista, list("x" = x_val)))))^2)
    }
  }
  else if(distribution == "Log-normal"){
    return(function(x) 1)
  }
  return(f)
}


#' Find Minimum Value
#'
#' @description
#' Searches the maximum of a function over a grid on a given grid.
#'
#' @param sens a single variable numeric function to evaluate.
#' @param min minimum value of the search grid.
#' @param max maximum value of the search grid.
#' @param grid.length length of the search grid.
#'
#' @return The value of the minimum
findminval <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  minval <- min(purrr::map_dbl(grid, sens))
  return(minval)
}

#' Find Maximum Value
#'
#' @description
#' Searches the maximum of a function over a grid on a given interval.
#'
#' @param sens A single variable numeric function to evaluate.
#' @param min Minimum value of the search interval.
#' @param max Maximum value of the search interval.
#' @param grid.length Length of the search interval.
#'
#' @return The value of the maximum
findmaxval <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  maxval <- max(purrr::map_dbl(grid, sens))
  return(maxval)
}


#' Find Maximum
#'
#' @description
#' Searches the maximum of a function over a grid on a given interval.
#'
#' @param sens A single variable numeric function to evaluate.
#' @param min Minimum value of the search interval.
#' @param max Maximum value of the search interval.
#' @param grid.length Length of the search interval.
#'
#' @return The value at which the maximum is obtained
findmax <- function(sens, min, max, grid.length) {
  if (min <= max) {
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  xmax <- grid[which.max(purrr::map(grid, sens))]
  return(xmax)
}

#' Deletes duplicates points
#'
#' @description
#' Within a vector of points, deletes points that are close enough (less than the tol parameter).
#' Returns the points without the "duplicates"
#'
#' @param points Points to be updated
#' @param tol Tolerance for which two points are considered the same
#'
#' @return The points without duplicates
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


#' Find where the candidate points region starts
#'
#' @description
#' Given the crosspoints and the sensitivity function, this function
#' finds where the candidate points region starts, either on the extreme of
#' the space of the design or the first crosspoints
#'
#' @param cross Vector of crosspoints in the sensitivity function given an efficiency and weight
#' @param min Minimum of the space of the design
#' @param max Maximum of the space of the design
#' @param val Value of the sensitivity function at the crosspoints
#' @param sens_opt Sensitivity function
#'
#' @return True if the candidate points region starts on the minimum, False otherwise
#'
getStart <- function(cross, min, max, val, sens_opt){
  if(length(cross)==1){
    if(round(cross[1]-min, 6)==0){
      if((val < sens_opt((max+cross[1])/2))){
        start <- F
      }
      else{
        start <- T
      }
    }
    else{
      if((val < sens_opt((min+cross[1])/2))){
        start <- T
      }
      else{
        start <- F
      }
    }
  }
  else if(val < sens_opt((cross[2]+cross[1])/2)){
    start <- F
  }
  else {
    start <- T
  }
  return(start)
}


#' Parity of the crosspoints
#'
#' @description
#' Determines if the number of crosspoints is even or odd given the vector
#' of crosspoints
#'
#' @param cross Vector of crosspoints in the sensitivity function given an efficiency and weight
#'
#' @return True if the number of crosspoints is even, false otherwise
getPar <- function(cross){
  return(length(cross)%%2 == 0)
}

#' Give effective limits to candidate points region
#'
#' @description
#' Given the start of the candidates points region, the parity of the crosspoints
#' and the boundaries of the space of the design returns the effective limits of
#' the candidate points region. Those points, taken in pairs from the first to
#' the last delimit the region.
#'
#' @param cross Vector of crosspoints in the sensitivity function given an efficiency and weight
#' @param min Minimum of the space of the design
#' @param max Maximum of the space of the design
#' @param start Boolean that gives the effective start of the candidate points region
#' @param par Boolean with the parity of the region
#'
#' @return Vector of effective limits of the candidate points region. Taken
#' in pairs from the beginning delimit the region.
getCross2 <- function(cross, min, max, start, par){
  if(par & start){
    cross2 <- c(min, cross, max)
  }
  else if(par & !start){
    cross2 <- cross
  }
  else if(!par & start){
    cross2 <- c(min, cross)
  }
  else{
    cross2 <- c(cross, max)
  }
  return(cross2)
}




#' Update weight D-Optimality
#'
#' @description
#' Implementation of the weight update formula for D-Optimality used to optimize the weights of a design,
#' which is to be applied iteratively until no sizable changes happen.
#'
#' @param design Design to optimize the weights from. It's a dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param sens Sensibility function for the design and model.
#' @param k Number of parameters of the model.
#' @param delta A parameter of the algorithm that can be tuned. Must be \eqn{0< delta < 1}.
#'
#' @return returns the new weights of the design after one iteration.
update_weights <- function(design, sens, k, delta) {
  weights <- design$Weight * (purrr::map_dbl(design$Point, sens) / k)^delta
  return(weights / sum(weights))
}


#' Update weight Ds-Optimality
#'
#' @description
#' Implementation of the weight update formula for Ds-Optimality used to optimize the weights of a design,
#' which is to be applied iteratively until no sizable changes happen.
#'
#'
#' @param s number of parameters of interest of the model
#' @inheritParams update_weights
#'
#' @return returns the new weights of the design after one iteration.
update_weightsDS <- function(design, sens, s, delta) {
  weights <- design$Weight * (purrr::map_dbl(design$Point, sens) / s)^delta
  return(weights)
}

#' Update weight I-Optimality
#'
#' @description
#' Implementation of the weight update formula for I-Optimality used to optimize the weights of a design,
#' which is to be applied iteratively until no sizable changes happen. A-Optimality if instead of the
#' integral matrix the identity function is used.
#'
#'
#' @param crit Value of the criterion function for I-Optimality.
#' @inheritParams update_weights
#'
#' @return returns the new weights of the design after one iteration.
update_weightsI <- function(design, sens, crit, delta) {
  exponent <- function(a, pow) (abs(a)^pow)*sign(a)
  weights <- design$Weight * exponent((purrr::map_dbl(design$Point, sens) / crit), delta)
  # weights[is.nan(weights)] <- 0
  # weights <- weights/sum(weights)
  return(weights/sum(weights))
}


#' Update Design with new point
#'
#' @description
#' Updates a design adding a new point to it. If the added point is closer than \code{delta} to an existing
#' point of the design, the two points are merged together as their arithmetic average. Then updates the weights
#' to be equal to all points of the design.
#'
#' @param design Design to update. It's a dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param xmax The point to add as a numeric value.
#' @param delta Threshold which defines how close the new point has to be to any of the existing ones in order to
#'   merge with them.
#' @param new_weight Number with the weight for the new point.
#'
#' @return The updated design.
update_design <- function(design, xmax, delta, new_weight) {
  absdiff <- abs(design$Point - xmax) < delta
  design$Weight <- design$Weight * (1 - new_weight)
  if (any(absdiff)) {
    pos <- min(which(absdiff == TRUE))
    design$Point[[pos]] <- (design$Point[[pos]] + xmax) / 2
    design$Weight[[pos]] <- design$Weight[[pos]] + new_weight
  }
  else {
    design[nrow(design) + 1, ] <- c(xmax, new_weight)
  }
  # design$Weight <- rep(1 / nrow(design), nrow(design))
  return(design)
}


#' Merge close points of a design
#'
#' @description
#' Takes a design and merge together all points that are closer between them than a certain threshold \code{delta}.
#'
#' @param design The design to update. It's a dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param delta Threshold which defines how close two points have to be to any of the existing ones in order to
#'   merge with them.
#'
#' @return The updated design.
update_design_total <- function(design, delta) {
  updated <- FALSE
  finished <- FALSE
  while (!finished) {
    for (i in 1:(length(design$Point) - 1)) {
      absdiff <- abs(design$Point[-seq(1, i)] - design$Point[i]) < delta
      if (any(absdiff)) {
        updated <- TRUE
        design <- update_design(design[-i, ], design$Point[i], delta, design$Weight[i])
        break
      }
    }
    finished <- !updated
    updated <- FALSE
  }
  return(design)
}


#' Remove low weight points
#'
#' @description
#' Removes the points of a design with a weight lower than a threshold, \code{delta}, and distributes that weights
#' proportionally to the rest of the points.
#'
#' @param design The design from which to remove points as a dataframe with two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param delta The threshold from which the points with such a weight or lower will be removed.
#'
#' @return The design without the removed points.
delete_points <- function(design, delta) {
  updatedDesign <- design[design$Weight > delta, ]
  updatedDesign$Weight <- updatedDesign$Weight / sum(updatedDesign$Weight)
  return(updatedDesign)
}

# General auxiliar functions --------------------------
# Cálculo de la traza de una matriz
#' Trace
#'
#' @description
#' Return the mathematical trace of a matrix, the sum of its diagonal elements.
#'
#'
#' @param M The matrix from which to calculate the trace.
#'
#' @return The trace of the matrix.
tr <- function(M) {
  return(sum(diag(M)))
}


#' Plot sensitivity function
#'
#' @description
#' Plots the sensitivity function and the value of the Equivalence Theorem as an horizontal line, which helps
#' assess the optimality of the design of the given sensitivity function.
#'
#' @param min Minimum of the space of the design, used in the limits of the representation.
#' @param max Maximum of the space of the design, used in the limits of the representation.
#' @param sens_function A single variable function, the sensitivity function.
#' @param criterion_value A numeric value representing the other side of the inequality of the Equivalence Theorem.
#'
#' @return A `ggplot` object that represents the sensitivity function
plot_sens <- function(min, max, sens_function, criterion_value) {
  x <- y <- NULL
  grid <- seq(min, max, length.out = 10000)
  sens_grid <- purrr::map_dbl(grid, sens_function)

  sensibility <- ggplot2::ggplot(data = data.frame(x = grid, y = sens_grid), mapping = ggplot2::aes(x = x)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y), color = "steelblue3") +
    ggplot2::stat_function(fun = function(x) criterion_value, col = "goldenrod3") +
    ggplot2::xlim(min, max) +
    ggplot2::labs(x = "Design Space", y = "Sensitivity Function")
}

#' Plot Convergence of the algorithm
#'
#' @description
#' Plots the criterion value on each of the steps of the algorithm, both for optimizing weights and points,
#' against the total step number.
#'
#' @param convergence A dataframe with two columns:
#'   * \code{criteria} contains value of the criterion on each step.
#'   * \code{step} contains number of the step.
#'
#' @return A ggplot object with the \code{criteria} in the \code{y} axis and \code{step} in the \code{x} axis.
plot_convergence <- function(convergence) {
  step <- criteria <- NULL
  ggplot2::ggplot(data = convergence, ggplot2::aes(x = step, y = criteria)) +
    ggplot2::geom_line(color = "coral1") +
    ggplot2::labs(y = "criterion") +
    ggplot2::theme_bw()
}


#' Integrate IM
#'
#' @description
#' Integrates the information matrix over the region of interest to calculate matrix B to be used in I-Optimality
#' calculation.
#'
#' @param grad function of partial derivatives of the model.
#' @param k number of unknown parameters of the model.
#' @param reg_int optional numeric vector of two components with the bounds of the interest region for I-Optimality.
#'
#' @return The integrated information matrix.
integrate_reg_int <- function(grad, k, reg_int) {
  matrix_int <- 0 * diag(k)
  for (i in 1:k) {
    for (j in 1:k) {
      if (j >= i) {
        int_part <- function(x_value) {
          purrr::map_dbl(x_value, function(x_value) grad(x_value)[i] * grad(x_value)[j] / (reg_int[2] - reg_int[1]))
        }
        matrix_int[i, j] <- stats::integrate(int_part, lower = reg_int[1], upper = reg_int[2])$value
      }
      else {
        matrix_int[i, j] <- matrix_int[j, i]
      }
    }
  }
  return(matrix_int)
}


#' Summary function for optdes
#'
#' @param object An object of class \code{optdes}.
#' @param ... Possible extra arguments for the summary
#'
#' @export
#'
#' @examples
#' rri <- opt_des(Criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' summary(rri)
summary.optdes <- function(object, ...) {
  cat("Model: \n")
  print(attr(object, "model"))
  cat("and weight function: \n")
  print(attr(attr(object, "weight_fun"), "srcref"))
  cat("Optimal design for ", object$criterion, ":\n")
  print.data.frame(object$optdes, ...)
  cat("\n Minimum efficiency (Atwood): ", paste0(attr(object, "atwood"), "%"))
  cat("\n Criterion value: ", object$crit_value)
}


#' Print function for optdes
#'
#' @param x An object of class \code{optdes}.
#' @param ... Possible extra arguments for printing dataframes
#'
#' @export
#'
#' @examples
#' rri <- opt_des(Criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' print(rri)
print.optdes <- function(x, ...) {
  print.data.frame(x$optdes, ...)
}



#' Plot function for optdes
#'
#' @param x An object of class \code{optdes}.
#' @param ... Possible extra arguments for plotting dataframes
#'
#' @export
#'
#' @examples
#' rri <- opt_des(Criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' plot(rri)
plot.optdes <- function(x, ...) {
  Point <- Value <- Weight <- NULL
  x$optdes[["Value"]] <- rep(0, nrow(x$optdes))
  x$optdes[["Weight"]] <- round(x$optdes[["Weight"]], 2)
  p <- x$sens + ggplot2::geom_point(data = x$optdes, ggplot2::aes(x = Point, y = Value)
                                    , size = 4, shape = 16, color = "darkgreen") +
    ggplot2::geom_text(data = x$optdes, ggplot2::aes(x = Point, y = Value, label = Weight),
                       hjust=1.5, vjust=1.5) +
    ggplot2::labs(x = "Design Space", y = "Sensitivity Function")
  p
}

