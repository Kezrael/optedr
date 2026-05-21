# Auxiliar function for algorithms --------------------------------------



#' Weight function per distribution
#'
#' @param model formula describing the model to use. Must use x as the variable.
#' @param char_vars character vector with the parameters of the models, as written in the \code{formula}
#' @param values numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.
#' @param distribution character variable specifying the probability distribution of the response. Can be one of the following:
#'   * 'Normal', for normal homoscedastic (default)
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'
#' @return one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response.
#'
weight_function <- function(model, char_vars, values, distribution = "Normal") {
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  if(!(distribution %in% c("Poisson", "Gamma", "Logistic", #"log-normal",
                           "Normal"))){
    warning(crayon::yellow(cli::symbol$warning), " Not a valid distribution specified, using a normal homoscedastic")
    return(function(x) 1)
  }
  else if(distribution == "Normal"){
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
#' Searches the minimum of a function over a grid on the design space.
#'
#' @param sens A numeric function to evaluate (scalar for 1D, named numeric vector for multi-factor).
#' @param design_space Numeric vector \code{c(min, max)} for single-factor models, or a
#'   named list \code{list(x1 = c(min, max), ...)} for multi-factor models.
#' @param grid.length Number of grid points (1D) or LHS samples (multi-factor).
#'
#' @return The value of the minimum
findminval <- function(sens, design_space, grid.length) {
  if (is.numeric(design_space)) {
    # Backward-compat: numeric vector c(min, max) as used by augment functions
    lo <- min(design_space); hi <- max(design_space)
    return(min(purrr::map_dbl(seq(lo, hi, length.out = grid.length), sens)))
  }
  if (length(design_space) == 1L) {
    # Single-factor canonical list: list(x = c(min, max))
    bnds <- design_space[[1L]]
    lo <- min(bnds); hi <- max(bnds)
    return(min(purrr::map_dbl(seq(lo, hi, length.out = grid.length), sens)))
  }
  # Multi-factor: LHS grid
  pts <- lhs_sample(grid.length, design_space)
  min(apply(pts, 1L, function(x) as.numeric(sens(stats::setNames(x, names(design_space))))))
}

#' Find Maximum Value
#'
#' @description
#' Searches the maximum of a function over a grid on a given interval or design space.
#'
#' @param sens A numeric function to evaluate (scalar argument for 1D, named numeric
#'   vector for multi-factor).
#' @param design_space Numeric vector \code{c(min, max)} for single-factor models, or a
#'   named list \code{list(x1 = c(min, max), ...)} for multi-factor models.
#' @param grid.length Number of grid points (1D) or LHS samples (multi-factor).
#'
#' @return The value of the maximum
findmaxval <- function(sens, design_space, grid.length) {
  if (is.numeric(design_space)) {
    lo <- min(design_space); hi <- max(design_space)
    return(max(purrr::map_dbl(seq(lo, hi, length.out = grid.length), sens)))
  }
  if (length(design_space) == 1L) {
    bnds <- design_space[[1L]]
    lo <- min(bnds); hi <- max(bnds)
    return(max(purrr::map_dbl(seq(lo, hi, length.out = grid.length), sens)))
  }
  pts <- lhs_sample(grid.length, design_space)
  max(apply(pts, 1L, function(x) as.numeric(sens(stats::setNames(x, names(design_space))))))
}


#' Find Maximum
#'
#' @description
#' Searches the location of the maximum of a sensitivity function over the design space.
#' For single-factor models the search uses a regular grid followed by direct selection.
#' For multi-factor models a Latin Hypercube Sample is evaluated and then refined with
#' L-BFGS-B local optimisation from the \code{n_starts} best candidate points.
#'
#' @param sens Sensitivity function (scalar for 1D, named numeric vector for multi-factor).
#' @param design_space Numeric vector \code{c(min, max)} or named list.
#' @param grid.length Number of grid / LHS points for the initial sweep.
#' @param n_starts Number of local-optimisation restarts (multi-factor only).
#'
#' @return The design point (scalar or named numeric vector) at which \code{sens} is maximised.
findmax <- function(sens, design_space, grid.length, n_starts = 5L) {
  if (is.numeric(design_space)) {
    lo <- min(design_space); hi <- max(design_space)
    grid <- seq(lo, hi, length.out = grid.length)
    return(grid[which.max(purrr::map(grid, sens))])
  }
  if (length(design_space) == 1L) {
    # ── Single-factor (current behaviour) ────────────────────────────────────
    bnds <- design_space[[1L]]
    lo   <- min(bnds); hi <- max(bnds)
    grid <- seq(lo, hi, length.out = grid.length)
    return(grid[which.max(purrr::map(grid, sens))])
  }

  # ── Multi-factor ──────────────────────────────────────────────────────────
  d        <- length(design_space)
  n_lhs    <- max(grid.length, 20L^min(d, 3L))
  pts      <- lhs_sample(n_lhs, design_space)
  nms      <- names(design_space)
  sens_vec <- apply(pts, 1L,
                    function(x) as.numeric(sens(stats::setNames(x, nms))))

  # Refine the best n_starts candidates with L-BFGS-B
  n_starts  <- min(n_starts, n_lhs)
  top_idx   <- order(sens_vec, decreasing = TRUE)[seq_len(n_starts)]
  best_x    <- pts[top_idx[1L], ]
  best_val  <- sens_vec[top_idx[1L]]
  lower     <- sapply(design_space, `[`, 1L)
  upper     <- sapply(design_space, `[`, 2L)

  for (idx in top_idx) {
    opt <- tryCatch(
      stats::optim(
        par    = pts[idx, ],
        fn     = function(x) -as.numeric(sens(stats::setNames(x, nms))),
        method = "L-BFGS-B",
        lower  = lower,
        upper  = upper
      ),
      error = function(e) list(value = Inf, par = pts[idx, ])
    )
    if (-opt$value > best_val) { best_val <- -opt$value; best_x <- opt$par }
  }
  stats::setNames(best_x, nms)
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
  weights <- design$Weight * (.eval_sens_all(design, sens) / k)^delta
  w_sum   <- sum(weights)
  if (!is.finite(w_sum) || w_sum == 0)
    stop("Weight update produced non-finite values. ",
         "The model is likely not identifiable in all specified parameters -",
         "check for parameter redundancy (see warnings above).",
         call. = FALSE)
  return(weights / w_sum)
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
  weights <- design$Weight * (.eval_sens_all(design, sens) / s)^delta
  w_sum   <- sum(weights)
  if (!is.finite(w_sum) || w_sum == 0)
    stop("Weight update produced non-finite values. ",
         "The model is likely not identifiable in all specified parameters -",
         "check for parameter redundancy (see warnings above).",
         call. = FALSE)
  return(weights / w_sum)
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
  exponent <- function(a, pow) (abs(a)^pow) * sign(a)
  weights  <- design$Weight * exponent(.eval_sens_all(design, sens) / crit, delta)
  w_sum    <- sum(weights)
  if (!is.finite(w_sum) || w_sum == 0)
    stop("Weight update produced non-finite values. ",
         "The model is likely not identifiable in all specified parameters -",
         "check for parameter redundancy (see warnings above).",
         call. = FALSE)
  return(weights / w_sum)
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
  dcols <- coord_cols(design)
  design$Weight <- design$Weight * (1 - new_weight)

  if (identical(dcols, "Point")) {
    # ── Single-factor (current behaviour) ──────────────────────────────────
    absdiff <- abs(design$Point - xmax) < delta
    if (any(absdiff)) {
      pos <- min(which(absdiff))
      design$Point[[pos]] <- (design$Point[[pos]] + xmax) / 2
      design$Weight[[pos]] <- design$Weight[[pos]] + new_weight
    } else {
      design[nrow(design) + 1L, ] <- c(xmax, new_weight)
    }
  } else {
    # ── Multi-factor: Euclidean distance ───────────────────────────────────
    dists <- apply(as.matrix(design[, dcols, drop = FALSE]), 1L, function(row)
      sqrt(sum((row - xmax[dcols])^2)))
    if (any(dists < delta)) {
      pos <- which.min(dists)
      design[pos, dcols] <- (unlist(design[pos, dcols]) + xmax[dcols]) / 2
      design$Weight[[pos]] <- design$Weight[[pos]] + new_weight
    } else {
      new_row        <- as.data.frame(as.list(xmax[dcols]))
      new_row$Weight <- new_weight
      design         <- rbind(design, new_row)
    }
  }
  design
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
  dcols    <- coord_cols(design)
  use_1d   <- identical(dcols, "Point")
  updated  <- FALSE
  finished <- FALSE
  while (!finished) {
    n <- nrow(design)
    if (n < 2L) break
    for (i in seq_len(n - 1L)) {
      if (use_1d) {
        absdiff <- abs(design$Point[-seq_len(i)] - design$Point[i]) < delta
      } else {
        rest <- as.matrix(design[-seq_len(i), dcols, drop = FALSE])
        curr <- unlist(design[i, dcols])
        absdiff <- apply(rest, 1L, function(row) sqrt(sum((row - curr)^2))) < delta
      }
      if (any(absdiff)) {
        updated  <- TRUE
        x_i      <- if (use_1d) design$Point[i] else unlist(design[i, dcols])
        design   <- update_design(design[-i, ], x_i, delta, design$Weight[i])
        break
      }
    }
    finished <- !updated
    updated  <- FALSE
  }
  design
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


# Evaluates a sensitivity function at every design point.
# Works for both single-factor (design has "Point" column, sens accepts scalar)
# and multi-factor (design has x1, x2, ..., sens accepts named vector).
.eval_sens_all <- function(design, sens) {
  dcols <- coord_cols(design)
  if (identical(dcols, "Point")) {
    purrr::map_dbl(design$Point, sens)
  } else {
    vapply(seq_len(nrow(design)), function(i)
      as.numeric(sens(unlist(design[i, dcols]))), numeric(1L))
  }
}

# Validates the Atwood efficiency bound and warns when it falls outside [0, 100],
# which indicates a degenerate information matrix (model not identifiable or design
# is singular). A bound > 100 % is the silent-failure signature of A/I/L-optimality
# applied to a non-identifiable model (pseudoinverse causes the ET condition to
# trigger prematurely).
check_atwood <- function(atwood) {
  val <- as.numeric(atwood)
  # Allow 0.01 percentage-point tolerance to absorb floating-point rounding
  # near the exact convergence boundary of 0 % and 100 %.
  if (!is.finite(val) || val < -0.01 || val > 100.01) {
    warning(sprintf(
      paste0("The Atwood efficiency bound is %.4g%%, which is outside [0, 100]. ",
             "This indicates a degenerate information matrix -the model is likely ",
             "not identifiable in all specified parameters. ",
             "Check for parameter redundancy (see any preceding warnings)."),
      val
    ), call. = FALSE)
  }
}

# Stable inverse for symmetric positive definite matrices (information matrices).
# Primary path: Cholesky (fast, exact for SPD).
# Fallback: Moore-Penrose pseudoinverse via SVD with a warning, for cases where
# the matrix is theoretically invertible but numerically non-PD (e.g., extreme
# parameter scales). Uses only base R -no extra dependencies.
inv_spd <- function(M) {
  result <- tryCatch(chol2inv(chol(M)), error = function(e) NULL)
  if (!is.null(result)) return(result)
  s <- svd(M)
  tol <- max(dim(M)) * max(s$d) * .Machine$double.eps
  d_inv <- ifelse(s$d > tol, 1 / s$d, 0)
  n_zero <- sum(d_inv == 0)
  if (n_zero > 0) {
    warning(sprintf(
      paste0("The information matrix has %d near-zero singular value(s) ",
             "(rank %d out of %d). The model likely has %d fewer identifiable ",
             "parameter(s) than specified -check for parameter redundancy or collinearity."),
      n_zero, nrow(M) - n_zero, nrow(M), n_zero
    ), call. = FALSE)
  } else {
    warning(
      "Information matrix is not positive definite; falling back to Moore-Penrose pseudoinverse. ",
      "Results may be unreliable. Check that the design has enough distinct support points ",
      "and that model parameters are on similar scales.",
      call. = FALSE
    )
  }
  s$v %*% diag(d_inv) %*% t(s$u)
}


# Heatmap of the sensitivity function for two-factor designs.
# Intended for d=2; returns a ggplot object with support points and ET contour overlaid.
plot_sens_2d <- function(design_space, sens_fn, design_points, criterion_value) {
  sens <- wlabel <- NULL   # avoid R CMD check NOTE on ggplot2 aes variables
  dvars   <- names(design_space)
  n_grid  <- 40L
  grid_df <- do.call(expand.grid, lapply(design_space, function(ds)
    seq(ds[1L], ds[2L], length.out = n_grid)))
  grid_df$sens <- apply(grid_df, 1L, function(x)
    as.numeric(sens_fn(stats::setNames(x, dvars))))

  label_df         <- design_points
  label_df$wlabel  <- paste0(round(label_df$Weight, 2))

  x1s <- rlang::sym(dvars[1L]); x2s <- rlang::sym(dvars[2L])
  ggplot2::ggplot(grid_df, ggplot2::aes(x = !!x1s, y = !!x2s, fill = sens)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = "Sensitivity") +
    ggplot2::geom_contour(ggplot2::aes(z = sens, fill = NULL),
                          breaks = criterion_value, colour = "white", linewidth = 0.7) +
    ggplot2::geom_point(data = label_df,
                        ggplot2::aes(x = !!x1s, y = !!x2s, fill = NULL),
                        colour = "red", size = 3.5, shape = 16) +
    ggplot2::geom_text(data = label_df,
                       ggplot2::aes(x = !!x1s, y = !!x2s, label = wlabel, fill = NULL),
                       colour = "white", size = 3, vjust = -1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x       = dvars[1L],
                  y       = dvars[2L],
                  caption = paste("White contour: ET threshold =", round(criterion_value, 4)))
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
  if (is.numeric(reg_int) && length(reg_int) == 2L) {
    # ── Single-factor: exact quadrature (current implementation) ─────────────
    matrix_int <- 0 * diag(k)
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        if (j >= i) {
          int_part <- function(x_value) {
            purrr::map_dbl(x_value, function(x) {
              g <- grad(x)
              g[i] * g[j] / (reg_int[2L] - reg_int[1L])
            })
          }
          matrix_int[i, j] <- stats::integrate(int_part,
                                                lower = reg_int[1L],
                                                upper = reg_int[2L])$value
        } else {
          matrix_int[i, j] <- matrix_int[j, i]
        }
      }
    }
    return(matrix_int)
  }

  # ── Multi-factor: Monte Carlo integration ───────────────────────────────────
  # reg_int must be a named list with the same names as the design variables.
  if (!is.list(reg_int))
    stop("For multi-factor models, reg_int must be a named list, e.g. ",
         "list(x1 = c(0, 10), x2 = c(0, 5)).", call. = FALSE)
  dvars  <- attr(grad, "design_vars")
  if (is.null(dvars)) dvars <- names(reg_int)
  n_mc   <- 10000L
  pts    <- lhs_sample(n_mc, reg_int)           # n_mc x d matrix
  vol    <- prod(sapply(reg_int, diff))
  # Accumulate outer products: B = vol * E[g(x) g(x)^T]
  matrix_int <- Reduce("+", lapply(seq_len(n_mc), function(i) {
    g <- as.vector(grad(stats::setNames(pts[i, ], dvars)))
    outer(g, g)
  })) * (vol / n_mc)
  matrix_int
}


#' Summary function for optdes
#'
#' @param object An object of class \code{optdes}.
#' @param ... Possible extra arguments for the summary
#'
#' @export
#'
#' @examples
#' rri <- opt_des(criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' summary(rri)
summary.optdes <- function(object, ...) {
  dvars <- attr(attr(object, "gradient"), "design_vars")
  multi <- is_multifactor(dvars)

  cat("Model:\n")
  print(attr(object, "model"))

  wf     <- attr(object, "weight_fun")
  wf_src <- attr(wf, "srcref")
  cat("Weight function:\n")
  if (!is.null(wf_src)) {
    print(wf_src)
  } else {
    cat(paste(deparse(wf), collapse = "\n"), "\n")
  }

  if (multi) {
    ds <- attr(object, "design_space")
    cat(sprintf("\nDesign space (%d factors):\n", length(dvars)))
    for (v in dvars)
      cat(sprintf("  %-5s [%.4g, %.4g]\n", paste0(v, ":"), ds[[v]][1L], ds[[v]][2L]))
  }

  cat(sprintf("\nOptimal design for %s", object$criterion))
  if (multi) cat(sprintf(" (%d support points)", nrow(object$optdes)))
  cat(":\n")
  print.data.frame(object$optdes, ...)
  cat(sprintf("\nMinimum efficiency (Atwood): %s%%", object$atwood))
  cat(sprintf("\nCriterion value:             %g\n", object$crit_value))
}


#' Print function for optdes
#'
#' @param x An object of class \code{optdes}.
#' @param ... Possible extra arguments for printing dataframes
#'
#' @export
#'
#' @examples
#' rri <- opt_des(criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' print(rri)
print.optdes <- function(x, ...) {
  dvars <- attr(attr(x, "gradient"), "design_vars")
  if (is_multifactor(dvars)) {
    cat(sprintf("Optimal design for %s (%d factors):\n", x$criterion, length(dvars)))
  } else {
    cat(sprintf("Optimal design for %s:\n", x$criterion))
  }
  print.data.frame(x$optdes, ...)
}



#' Plot function for optdes
#'
#' @description
#' For single-factor models, overlays the support points on the sensitivity function curve.
#' For two-factor models, shows a heatmap of the sensitivity function with the support
#' points overlaid and the Equivalence Theorem contour highlighted.
#' For models with more than two factors, prints the design and returns it invisibly.
#'
#' @param x An object of class \code{optdes}.
#' @param ... Possible extra arguments (currently unused).
#'
#' @export
#'
#' @examples
#' rri <- opt_des(criterion = "I-Optimality", model = y ~ a * exp(-b / x),
#'   parameters = c("a", "b"), par_values = c(1, 1500), design_space = c(212, 422),
#'   reg_int = c(380, 422))
#' plot(rri)
plot.optdes <- function(x, ...) {
  dcols <- coord_cols(x$optdes)

  if (identical(dcols, "Point")) {
    # ── Single-factor: sensitivity curve + support points ──────────────────
    Point <- Value <- Weight <- NULL
    x$optdes[["Value"]]  <- rep(0, nrow(x$optdes))
    x$optdes[["Weight"]] <- round(x$optdes[["Weight"]], 2)
    p <- x$sens +
      ggplot2::geom_point(data = x$optdes, ggplot2::aes(x = Point, y = Value),
                          size = 4, shape = 16, color = "darkgreen") +
      ggplot2::geom_text(data = x$optdes, ggplot2::aes(x = Point, y = Value, label = Weight),
                         hjust = 1.5, vjust = 1.5) +
      ggplot2::labs(x = "Design Space", y = "Sensitivity Function")
    return(p)
  }

  if (length(dcols) == 2L) {
    # ── Two-factor: sensitivity heatmap with support points overlaid ───────
    # x$sens is pre-computed by the cocktail algorithm and already includes
    # the support points and ET contour — return it directly.
    return(x$sens)
  }

  # ── d > 2: just print ──────────────────────────────────────────────────
  message("Visualisation for designs with more than 2 factors is not implemented. ",
          "Use print() or summary() to inspect the design.")
  invisible(x$optdes)
}

