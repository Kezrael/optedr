# Auxiliar function for algorithms --------------------------------------


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
#'
#' @examples
#' optedr:::findmax(function(x) x^2 - x^3, 0, 1, 1000)
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
#' @param delta A parameter of the algorithm that can be tunned. Must be \eqn{0< delta < 1}.
#'
#' @return returns the new weights of the design after one iteration.
#'
#' @examples
#' \dontrun{
#' updateWeights(design, sens, k, delta)
#' }
updateWeights <- function(design, sens, k, delta) {
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
#' @param s Number of interest parameters of the model
#' @inheritParams updateWeights
#'
#' @return returns the new weights of the design after one iteration.
#'
#' @examples
#' \dontrun{
#' updateWeightsDS(design, sens, s, delta)
#' }
updateWeightsDS <- function(design, sens, s, delta) {
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
#' @inheritParams updateWeights
#'
#' @return returns the new weights of the design after one iteration.
#'
#'
#' @examples
#' \dontrun{
#' updateWeightsI(design, sens, crit, delta)
#' }
updateWeightsI <- function(design, sens, crit, delta) {
  weights <- design$Weight * (purrr::map_dbl(design$Point, sens) / crit)^delta
  return(weights)
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
#'
#' @return The updated design.
#'
#' @examples
#' # Without merging:
#' design <- data.frame("Point" = c(1, 5, 9), "Weight" = rep(1 / 3, times = 3))
#' optedr:::updateDesign(design, 2, 0.5)
#'
#' # Merging:
#' design <- data.frame("Point" = c(1, 5, 9), "Weight" = rep(1 / 3, times = 3))
#' optedr:::updateDesign(design, 2, 1.1)
updateDesign <- function(design, xmax, delta) {
  absdiff <- abs(design$Point - xmax) < delta
  if (any(absdiff)) {
    pos <- min(which(absdiff == TRUE))
    design$Point[[pos]] <- (design$Point[[pos]] + xmax) / 2
  }
  else {
    design[nrow(design) + 1, ] <- c(xmax, 1 / (nrow(design) + 1))
  }
  design$Weight <- rep(1 / nrow(design), nrow(design))
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
#'
#' @examples
#' design <- data.frame("Point" = c(1, 5, 11, 12), "Weight" = rep(1 / 4, times = 4))
#' optedr:::updateDesignTotal(design, 1.1)
updateDesignTotal <- function(design, delta) {
  updated <- FALSE
  finished <- FALSE
  while (!finished) {
    for (i in 1:(length(design$Point) - 1)) {
      absdiff <- abs(design$Point[-seq(1, i)] - design$Point[i]) < delta
      if (any(absdiff)) {
        updated <- TRUE
        design <- updateDesign(design[-i, ], design$Point[i], delta)
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
#'
#'
#' @examples
#' design <- data.frame("Point" = seq(1, 100, length.out = 8), "Weight" = c(0.3, 0.05, 0.3, 0.03, 0.1, 0.1, 0.02, 0.1))
#' optedr:::deletePoints(design, 0.09)
deletePoints <- function(design, delta) {
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
#'
#' @examples
#' optedr:::tr(matrix(c(1, 0, 0, 1), nrow = 2))
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
#'
#' @examples
#' \dontrun{
#' plot_sens(1, 100, sens, crit)
#' }
plot_sens <- function(min, max, sens_function, criterion_value) {
  grid <- seq(min, max, length.out = 10000)
  sens_grid <- purrr::map_dbl(grid, sens_function)

  sensibility <- ggplot2::ggplot(data = data.frame(x = grid, y = sens_grid), mapping = ggplot2::aes(x = x)) +
    ggplot2::theme_bw() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y), color = "steelblue3") +
    ggplot2::stat_function(fun = function(x) criterion_value, col = "goldenrod3") +
    ggplot2::xlim(min, max) +
    ggplot2::labs(x = "X", y = "Y")
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
#'
#' @examples
#' conv <- data.frame("criteria" = c(24, 23, 20, 15, 14.9, 14.8, 14.7), "step" = 1:7)
#' optedr:::plot_convergence(conv)
plot_convergence <- function(convergence) {
  ggplot2::ggplot(data = convergence, ggplot2::aes(x = step, y = criteria)) +
    ggplot2::geom_line(color = "coral1") +
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
#'
#'
#' @examples
#' grad <- optedr:::gradient(y ~ a + b * x, c("a", "b"), c(1, 1))
#' optedr:::integrate_reg_int(grad, 2, c(0, 3))
integrate_reg_int <- function(grad, k, reg_int) {
  matrix_int <- 0 * diag(k)
  for (i in 1:k) {
    for (j in 1:k) {
      if (j >= i) {
        int_part <- function(x_value) {
          purrr::map_dbl(x_value, function(x_value) grad(x_value)[i] * grad(x_value)[j] / (reg_int[2] - reg_int[1]))
        }
        matrix_int[i, j] <- integrate(int_part, lower = reg_int[1], upper = reg_int[2])$value
      }
      else {
        matrix_int[i, j] <- matrix_int[j, i]
      }
    }
  }
  return(matrix_int)
}


#' Print function for optdes
#'
#' @param x An object of class \code{optdes}.
#'
#' @export
#'
#' @examples
#' rri <- opt_des("I-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422), matB = matrix(c(3, 1, 1, 2), nrow = 2))
#' print(rri)
print.optdes <- function(x) {
  cat("Optimal design for ", x$criterion, ":\n")
  print.data.frame(x$optdes)
  cat("\n Criterion value: ", x$crit_value)
}
