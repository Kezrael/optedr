#' Master function for the cocktail algorithm, that calls the appropriate one given the criterion.
#'
#' @description
#' Depending on the \code{criterion} the cocktail algorithm for the chosen criterion is called,
#' and the necessary parameters for the functions are given from the user input.
#'
#' @param init_design optional dataframe with the initial design for the algorithm.
#' @param grad function of partial derivatives of the model.
#' @param criterion character variable with the chosen optimality criterion.
#' @param par_int numeric vector with the index of the parameters of interest (Ds-Optimality).
#' @param matB optional matrix for L-optimality.
#' @param design_space named list with bounds for each design variable.
#' @param grid.length numeric value for the sensitivity search grid / LHS size.
#' @param join_thresh numeric value for the merge heuristic.
#' @param delete_thresh numeric value for the weight deletion threshold.
#' @param k number of unknown parameters of the model.
#' @param delta_weights numeric value in (0, 1), parameter of the algorithm.
#' @param tol numeric value for convergence of the weight loop.
#' @param tol2 numeric value for the outer stop condition.
#' @param max_iter maximum number of outer iterations.
#' @param compound preprocessed list of compound criterion specifications (internal use).
#' @param kl_spec preprocessed list of KL-Optimality specifications (internal use).
#'
#' @return An object of class \code{optdes}.
#'
#' @family cocktail algorithms
WFMult <- function(init_design, grad, criterion, par_int = NA, matB = NA,
                   design_space, grid.length, join_thresh, delete_thresh,
                   k, delta_weights, tol, tol2, max_iter, compound = NULL,
                   kl_spec = NULL) {
  if (identical(criterion, "D-Optimality"))
    return(DWFMult(init_design, grad, design_space, grid.length,
                   join_thresh, delete_thresh, k, delta_weights, tol, tol2, max_iter))
  else if (identical(criterion, "Ds-Optimality"))
    return(DsWFMult(init_design, grad, par_int, design_space, grid.length,
                    join_thresh, delete_thresh, delta_weights, tol, tol2, max_iter))
  else if (identical(criterion, "A-Optimality"))
    return(IWFMult(init_design, grad, diag(k), design_space, grid.length,
                   join_thresh, delete_thresh, delta_weights, tol, tol2, "A-Optimality", max_iter))
  else if (identical(criterion, "I-Optimality"))
    return(IWFMult(init_design, grad, matB, design_space, grid.length,
                   join_thresh, delete_thresh, delta_weights, tol, tol2, "I-Optimality", max_iter))
  else if (identical(criterion, "L-Optimality"))
    return(IWFMult(init_design, grad, matB, design_space, grid.length,
                   join_thresh, delete_thresh, delta_weights, tol, tol2, "L-Optimality", max_iter))
  else if (identical(criterion, "Compound"))
    return(CWFMult(init_design, grad, compound, design_space, grid.length,
                   join_thresh, delete_thresh, delta_weights, tol, tol2, max_iter))
  else if (identical(criterion, "KL-Optimality"))
    return(KLWFMult(init_design,
                    kl_spec$kl_fun, kl_spec$beta2_init,
                    kl_spec$lower, kl_spec$upper,
                    design_space, grid.length,
                    join_thresh, delete_thresh,
                    delta_weights, tol, tol2, max_iter,
                    kl_meta = kl_spec$kl_meta))
}


#' Cocktail Algorithm implementation for D-Optimality
#'
#' @inherit WFMult return params
#' @family cocktail algorithms
DWFMult <- function(init_design, grad, design_space, grid.length,
                    join_thresh, delete_thresh, k, delta_weights, tol, tol2, max_iter) {
  multi   <- is_multifactor(attr(grad, "design_vars"))
  dc      <- coord_cols(init_design)
  maxiter_weights <- 100L
  crit_val <- numeric(max_iter * (maxiter_weights + 1L) + 1L)
  index <- 1L
  cli::cli_progress_bar("Calculating optimal design")

  for (i in seq_len(max_iter)) {
    cli::cli_progress_update()
    M     <- inf_mat(grad, init_design)
    crit_val[index] <- dcrit(M, k)
    index <- index + 1L
    sensM <- dsens(grad, M)
    xmax  <- findmax(sensM, design_space, grid.length)
    if ((as.numeric(sensM(xmax)) - k) / k < tol2) {
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1L; stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      M     <- inf_mat(grad, init_design)
      crit_val[index] <- dcrit(M, k)
      index <- index + 1L
      sensM <- dsens(grad, M)
      init_design$Weight <- update_weights(init_design, sensM, k, delta_weights)
      stopw <- max(abs(weightsInit - init_design$Weight)) < tol || iter >= maxiter_weights
      iter  <- iter + 1L
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) init_design <- update_design_total(init_design, join_thresh)
    if (i == max_iter)
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition not reached, max iterations performed")
  }

  cli::cli_progress_update(force = TRUE)
  base::cat("")
  crit_val[index] <- dcrit(M, k)
  crit_val <- crit_val[1L:(length(crit_val) - sum(crit_val == 0))]
  conv_plot <- plot_convergence(data.frame("criteria" = crit_val,
                                           "step"     = seq_along(crit_val)))

  init_design <- init_design[order(init_design[[dc[1L]]]), ]
  rownames(init_design) <- NULL

  M     <- inf_mat(grad, init_design)
  sensM <- dsens(grad, M)
  xmax  <- findmax(sensM, design_space, grid.length * 10L)
  atwood <- k / as.numeric(sensM(xmax)) * 100
  check_atwood(atwood)
  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- .make_sens_plot(multi, design_space, sensM, dc, init_design, k)

  l_return <- list("optdes" = init_design, "convergence" = conv_plot, "sens" = plot_opt,
                   "criterion" = "D-Optimality", "crit_value" = crit_val[length(crit_val)],
                   "atwood" = atwood)
  attr(l_return, "hidden_value")  <- k
  attr(l_return, "gradient")      <- grad
  attr(l_return, "design_space")  <- design_space
  attr(l_return, "crit_function") <- function(design) { M <- inf_mat(grad, design); dcrit(M, k) }
  class(l_return) <- "optdes"
  l_return
}


#' Cocktail Algorithm implementation for Ds-Optimality
#'
#' @inherit WFMult return params
#' @family cocktail algorithms
DsWFMult <- function(init_design, grad, par_int, design_space, grid.length,
                     join_thresh, delete_thresh, delta_weights, tol, tol2, max_iter) {
  multi   <- is_multifactor(attr(grad, "design_vars"))
  dc      <- coord_cols(init_design)
  maxiter_weights <- 100L
  crit_val <- numeric(max_iter * (maxiter_weights + 1L) + 1L)
  index <- 1L
  cli::cli_progress_bar("Calculating optimal design")

  for (i in seq_len(max_iter)) {
    cli::cli_progress_update()
    M      <- inf_mat(grad, init_design)
    crit_val[index] <- dscrit(M, par_int)
    index <- index + 1L
    sensDs <- dssens(grad, M, par_int)
    xmax   <- findmax(sensDs, design_space, grid.length)
    if ((as.numeric(sensDs(xmax)) - length(par_int)) / length(par_int) < tol2) {
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1L; stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      crit_val[index] <- dscrit(M, par_int)
      index <- index + 1L
      M      <- inf_mat(grad, init_design)
      sensDs <- dssens(grad, M, par_int)
      init_design$Weight <- update_weightsDS(init_design, sensDs, length(par_int), delta_weights)
      stopw <- max(abs(weightsInit - init_design$Weight)) < tol || iter >= maxiter_weights
      iter  <- iter + 1L
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) init_design <- update_design_total(init_design, join_thresh)
    if (i == max_iter)
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition not reached, max iterations performed")
  }

  cli::cli_progress_update(force = TRUE)
  crit_val[index] <- dscrit(M, par_int)
  crit_val <- crit_val[1L:(length(crit_val) - sum(crit_val == 0))]
  conv_plot <- plot_convergence(data.frame("criteria" = crit_val,
                                           "step"     = seq_along(crit_val)))

  init_design <- init_design[order(init_design[[dc[1L]]]), ]
  rownames(init_design) <- NULL

  M      <- inf_mat(grad, init_design)
  sensM  <- dssens(grad, M, par_int)
  xmax   <- findmax(sensM, design_space, grid.length * 10L)
  atwood <- (2 - as.numeric(sensM(xmax)) / length(par_int)) * 100
  check_atwood(atwood)
  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- .make_sens_plot(multi, design_space, sensM, dc, init_design, length(par_int))

  l_return <- list("optdes" = init_design, "convergence" = conv_plot, "sens" = plot_opt,
                   "criterion" = "Ds-Optimality", "crit_value" = crit_val[length(crit_val)],
                   "atwood" = atwood)
  attr(l_return, "hidden_value")  <- par_int
  attr(l_return, "gradient")      <- grad
  attr(l_return, "design_space")  <- design_space
  attr(l_return, "crit_function") <- function(design) { M <- inf_mat(grad, design); dscrit(M, par_int) }
  class(l_return) <- "optdes"
  l_return
}


#' Cocktail Algorithm implementation for L-, I- and A-Optimality
#'
#' @inherit WFMult return params
#' @family cocktail algorithms
IWFMult <- function(init_design, grad, matB, design_space, grid.length,
                    join_thresh, delete_thresh, delta_weights, tol, tol2, criterion, max_iter) {
  multi   <- is_multifactor(attr(grad, "design_vars"))
  dc      <- coord_cols(init_design)
  maxiter_weights <- 100L
  crit_val <- numeric(max_iter * (maxiter_weights + 1L) + 1L)
  index <- 1L
  cli::cli_progress_bar("Calculating optimal design")

  for (i in seq_len(max_iter)) {
    cli::cli_progress_update()
    M     <- inf_mat(grad, init_design)
    crit_val[index] <- icrit(M, matB)
    index <- index + 1L
    sensI <- isens(grad, M, matB)
    xmax  <- findmax(sensI, design_space, grid.length)
    if ((as.numeric(sensI(xmax)) - crit_val[index - 1L]) < tol2) {
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1L; stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      M     <- inf_mat(grad, init_design)
      crit  <- icrit(M, matB)
      crit_val[index] <- crit
      index <- index + 1L
      sensI <- isens(grad, M, matB)
      init_design$Weight <- update_weightsI(init_design, sensI, crit, delta_weights)
      stopw <- max(abs(weightsInit - init_design$Weight)) < tol || iter >= maxiter_weights
      iter  <- iter + 1L
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) init_design <- update_design_total(init_design, join_thresh)
    if (i == max_iter)
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition not reached, max iterations performed")
  }

  cli::cli_progress_update(force = TRUE)
  M     <- inf_mat(grad, init_design)
  crit_val[index] <- icrit(M, matB)
  crit_val <- crit_val[1L:(length(crit_val) - sum(crit_val == 0))]
  conv_plot <- plot_convergence(data.frame("criteria" = crit_val,
                                           "step"     = seq_along(crit_val)))

  init_design <- init_design[order(init_design[[dc[1L]]]), ]
  rownames(init_design) <- NULL

  M     <- inf_mat(grad, init_design)
  sensM <- isens(grad, M, matB)
  xmax  <- findmax(sensM, design_space, grid.length * 10L)
  atwood <- (2 - as.numeric(sensM(xmax)) / icrit(M, matB)) * 100
  check_atwood(atwood)
  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- .make_sens_plot(multi, design_space, sensM, dc, init_design, icrit(M, matB))

  l_return <- list("optdes" = init_design, "convergence" = conv_plot, "sens" = plot_opt,
                   "criterion" = criterion, "crit_value" = crit_val[length(crit_val)],
                   "atwood" = atwood)
  attr(l_return, "hidden_value")  <- matB
  attr(l_return, "gradient")      <- grad
  attr(l_return, "design_space")  <- design_space
  attr(l_return, "crit_function") <- function(design) { M <- inf_mat(grad, design); icrit(M, matB) }
  class(l_return) <- "optdes"
  l_return
}


#' Cocktail Algorithm implementation for Compound Optimality
#'
#' @inherit WFMult return params
#' @param compound_specs preprocessed list of per-criterion specifications.
#' @family cocktail algorithms
CWFMult <- function(init_design, grad, compound_specs, design_space, grid.length,
                    join_thresh, delete_thresh, delta_weights, tol, tol2, max_iter) {
  multi   <- is_multifactor(attr(grad, "design_vars"))
  dc      <- coord_cols(init_design)
  maxiter_weights <- 100L
  crit_val <- numeric(max_iter * (maxiter_weights + 1L) + 1L)
  index <- 1L
  cli::cli_progress_bar("Calculating optimal design")

  for (i in seq_len(max_iter)) {
    cli::cli_progress_update()
    M     <- inf_mat(grad, init_design)
    crit_val[index] <- ccrit(compound_specs, M)
    index <- index + 1L
    sensC <- csens(compound_specs, grad, M)
    thrC  <- compound_threshold(compound_specs, M)
    xmax  <- findmax(sensC, design_space, grid.length)
    if ((as.numeric(sensC(xmax)) - thrC) < tol2) {
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1/(index + 2))
    iter <- 1L; stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      M     <- inf_mat(grad, init_design)
      crit_val[index] <- ccrit(compound_specs, M)
      index <- index + 1L
      sensC <- csens(compound_specs, grad, M)
      thrC  <- compound_threshold(compound_specs, M)
      init_design$Weight <- update_weightsI(init_design, sensC, thrC, delta_weights)
      stopw <- max(abs(weightsInit - init_design$Weight)) < tol || iter >= maxiter_weights
      iter  <- iter + 1L
    }
    init_design <- delete_points(init_design, delete_thresh)
    if (i %% 5 == 0) init_design <- update_design_total(init_design, join_thresh)
    if (i == max_iter)
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition not reached, max iterations performed")
  }

  cli::cli_progress_update(force = TRUE)
  M     <- inf_mat(grad, init_design)
  crit_val[index] <- ccrit(compound_specs, M)
  crit_val <- crit_val[1L:(length(crit_val) - sum(crit_val == 0))]
  conv_plot <- plot_convergence(data.frame("criteria" = crit_val,
                                           "step"     = seq_along(crit_val)))

  init_design <- init_design[order(init_design[[dc[1L]]]), ]
  rownames(init_design) <- NULL

  M     <- inf_mat(grad, init_design)
  sensM <- csens(compound_specs, grad, M)
  xmax  <- findmax(sensM, design_space, grid.length * 10L)
  thrC  <- compound_threshold(compound_specs, M)
  atwood <- thrC / as.numeric(sensM(xmax)) * 100
  check_atwood(atwood)
  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- .make_sens_plot(multi, design_space, sensM, dc, init_design, thrC)

  l_return <- list("optdes"      = init_design,
                   "convergence" = conv_plot,
                   "sens"        = plot_opt,
                   "criterion"   = "Compound",
                   "crit_value"  = crit_val[length(crit_val)],
                   "atwood"      = atwood)
  attr(l_return, "hidden_value")  <- compound_specs
  attr(l_return, "gradient")      <- grad
  attr(l_return, "design_space")  <- design_space
  attr(l_return, "crit_function") <- function(design) { M <- inf_mat(grad, design); ccrit(compound_specs, M) }
  class(l_return) <- "optdes"
  l_return
}


# Dispatch plot creation based on design dimensionality.
# 1D: sensitivity curve; d=2: heatmap; d>2: NULL.
.make_sens_plot <- function(multi, design_space, sens_fn, dc, design, criterion_value) {
  if (!multi) {
    plot_sens(design_space[[1L]][1L], design_space[[1L]][2L], sens_fn, criterion_value)
  } else if (length(design_space) == 2L) {
    plot_sens_2d(design_space, sens_fn, design[, c(dc, "Weight"), drop = FALSE], criterion_value)
  } else {
    NULL
  }
}


#' Calculates the optimal design for a specified criterion
#'
#' @description
#' The opt_des function calculates the optimal design for an optimality criterion and a model input from the user.
#' Supports single-factor models (design variable named \code{x}) and multi-factor models
#' (design variables named \code{x1}, \code{x2}, \ldots, detected automatically from the formula).
#'
#' @param criterion character variable with the chosen optimality criterion.
#' @param model formula describing the model. For single-factor models use \code{x} as the
#'   design variable; for multi-factor models use \code{x1}, \code{x2}, etc.
#' @param parameters character vector with the parameter names.
#' @param par_values numeric vector with the nominal parameter values.
#' @param design_space For single-factor models: numeric vector \code{c(min, max)}.
#'   For multi-factor models: named list \code{list(x1 = c(min, max), x2 = c(min, max), ...)}.
#' @param init_design optional dataframe with the initial design. For single-factor models
#'   use columns \code{Point} and \code{Weight}; for multi-factor models use one column per
#'   design variable plus \code{Weight}.
#' @param join_thresh optional numeric threshold for merging nearby design points.
#' @param delete_thresh optional numeric minimum weight to keep a support point.
#' @param delta optional numeric in (0, 1), damping parameter of the algorithm.
#' @param tol optional numeric for convergence of the weight loop.
#' @param tol2 optional numeric for the outer stop condition.
#' @param par_int optional numeric vector of parameter indices for Ds-optimality.
#' @param matB optional k x k matrix for L-optimality.
#' @param reg_int optional bounds for the I-optimality integration region.
#'   Single-factor: \code{c(min, max)}.  Multi-factor: named list matching \code{design_space}.
#' @param max_iter optional integer maximum number of outer cocktail iterations.
#' @param distribution character variable specifying the response distribution.
#' @param weight_fun optional variance-structure weight function.
#' @param compound optional list of criterion specifications for \code{criterion = "Compound"}.
#'   Each element must be a named list with at least \code{criterion} (character) and
#'   \code{weight} (positive numeric). Additional fields per sub-criterion:
#'   \code{par_int} (Ds-Optimality), \code{reg_int} (I-Optimality), \code{matB} (L-Optimality).
#'   Weights are normalised to sum to 1. Example:
#'   \code{list(list(criterion="D-Optimality", weight=0.7),
#'              list(criterion="I-Optimality", weight=0.3, reg_int=c(380,422)))}.
#' @param rival_model optional formula for the rival model used with
#'   \code{criterion = "KL-Optimality"}. Defaults to \code{model} (same structure, rival
#'   parameters explored by inner optimisation).
#' @param rival_params optional character vector of rival model parameter names. Defaults to
#'   \code{parameters}.
#' @param rival_pars optional numeric vector of initial rival parameter values for the inner
#'   optimisation. Defaults to \code{par_values}.
#' @param family character; GLM family for \code{criterion = "KL-Optimality"}. One of
#'   \code{"Normal"}, \code{"Poisson"}, \code{"Binomial"}, \code{"Gamma"}. Default \code{"Normal"}.
#' @param phi positive numeric dispersion parameter for KL-Optimality. Default \code{1}.
#' @param rival_lower optional numeric vector of lower bounds for rival parameters in the
#'   inner optimisation. Defaults to \code{-Inf} for each parameter.
#' @param rival_upper optional numeric vector of upper bounds for rival parameters in the
#'   inner optimisation. Defaults to \code{Inf} for each parameter.
#'
#' @return a list of class \code{optdes} with components \code{optdes}, \code{convergence},
#'   \code{sens}, \code{criterion}, \code{crit_value}, and \code{atwood}.
#' @export
#'
#' @examples
#' # Single-factor (backward compatible)
#' opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
#'
#' \donttest{
#' # Two-factor Michaelis-Menten bisubstrate model
#' opt_des("D-Optimality",
#'         y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
#'         c("Vmax", "K1", "K2"), c(1, 1, 1),
#'         list(x1 = c(0.1, 10), x2 = c(0.1, 10)))
#'
#' # Compound D+I (70% D, 30% I) for Antoine equation
#' opt_des("Compound",
#'         y ~ 10^(a - b/(c + x)), c("a","b","c"),
#'         c(8.07131, 1730.63, 233.426), c(1, 100),
#'         compound = list(
#'           list(criterion = "D-Optimality", weight = 0.7),
#'           list(criterion = "I-Optimality", weight = 0.3, reg_int = c(60, 100))
#'         ))
#'
#' # KL-Optimality: discriminate quadratic from linear mean model (Normal)
#' opt_des("KL-Optimality",
#'         model        = y ~ a * x^2,
#'         parameters   = c("a"),
#'         par_values   = c(1),
#'         design_space = c(1, 5),
#'         rival_model  = y ~ b * x,
#'         rival_params = c("b"),
#'         rival_pars   = c(3),
#'         family       = "Normal",
#'         phi          = 1)
#' }
opt_des <- function(criterion, model, parameters,
                    par_values   = c(1),
                    design_space,
                    init_design  = NULL,
                    join_thresh  = -1,
                    delete_thresh = 0.02,
                    delta        = 1 / 2,
                    tol          = 0.00001,
                    tol2         = 0.00001,
                    par_int      = NULL,
                    matB         = NULL,
                    reg_int      = NULL,
                    max_iter     = 21L,
                    distribution = NA,
                    weight_fun   = function(x) 1,
                    compound     = NULL,
                    rival_model  = NULL,
                    rival_params = NULL,
                    rival_pars   = NULL,
                    family       = "Normal",
                    phi          = 1,
                    rival_lower  = NULL,
                    rival_upper  = NULL,
                    kl_fun       = NULL) {

  k <- length(parameters)
  if (identical(par_values, c(1))) par_values <- rep(1, k)

  # -- Auto-detect design variables and normalise design_space --------------
  design_vars  <- detect_design_vars(model, parameters)
  multi        <- is_multifactor(design_vars)
  design_space <- canonicalise_design_space(design_space, design_vars)

  # For single-factor: also accept reversed bounds
  if (!multi) {
    bnds <- design_space[[1L]]
    if (bnds[1L] > bnds[2L]) design_space[[1L]] <- rev(bnds)
  }

  # -- Initial design --------------------------------------------------------
  n0 <- k * (k + 1L) / 2L + 1L
  if (is.null(init_design)) {
    if (!multi) {
      bnds <- design_space[[1L]]
      init_design <- data.frame(
        "Point"  = seq(bnds[1L], bnds[2L], length.out = n0),
        "Weight" = rep(1 / n0, n0)
      )
    } else {
      pts <- lhs_sample(n0, design_space)
      init_design <- as.data.frame(pts)
      init_design$Weight <- 1 / n0
    }
  } else {
    # Normalise user-supplied design column names (accepts "Point" for 1D)
    init_design <- normalize_design_cols(init_design, design_vars)
  }

  # -- Input validation ------------------------------------------------------
  check_inputs(
    criterion, model, parameters, par_values, design_space, init_design,
    join_thresh, delete_thresh, delta, tol, tol2, par_int, matB, reg_int, weight_fun
  )

  # -- Weight function and gradient ------------------------------------------
  if (!is.na(distribution))
    weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
  grad <- gradient(model, parameters, par_values, weight_fun)

  # -- join_thresh default ---------------------------------------------------
  if (join_thresh == -1)
    join_thresh <- min(sapply(design_space, diff)) / 10

  # -- I-Optimality: build matB ----------------------------------------------
  if (identical(criterion, "I-Optimality") && !is.null(reg_int))
    matB <- integrate_reg_int(grad, k, reg_int)

  # -- Compound: preprocess specs (normalise weights, build matB per component) -
  compound_specs <- NULL
  if (identical(criterion, "Compound")) {
    if (is.null(compound) || length(compound) < 2L)
      stop("criterion = 'Compound' requires compound list with at least 2 specifications.",
           call. = FALSE)
    ws <- sapply(compound, function(s) {
      if (is.null(s$weight) || !is.numeric(s$weight) || s$weight <= 0)
        stop("Each compound element must have a positive numeric 'weight'.", call. = FALSE)
      s$weight
    })
    # Warn if weights don't already sum to 1 (before normalising)
    if (abs(sum(ws) - 1) > 1e-8)
      message(crayon::yellow(cli::symbol$warning),
              " compound weights sum to ", round(sum(ws), 6),
              " - normalising to 1.")
    ws <- ws / sum(ws)

    # Warn about duplicate components (same criterion + same auxiliary params)
    .compound_key <- function(s) {
      key <- s$criterion
      if (identical(s$criterion, "Ds-Optimality") && !is.null(s$par_int))
        key <- paste0(key, ":par_int=", paste(sort(s$par_int), collapse=","))
      if (identical(s$criterion, "I-Optimality")  && !is.null(s$reg_int))
        key <- paste0(key, ":reg_int=", paste(s$reg_int, collapse=","))
      if (identical(s$criterion, "L-Optimality")  && !is.null(s$matB))
        key <- paste0(key, ":matB=<custom>")
      key
    }
    keys <- sapply(compound, .compound_key)
    dups <- keys[duplicated(keys)]
    if (length(dups) > 0L)
      warning("Duplicate compound components detected (", paste(unique(dups), collapse=", "),
              "). Their weights will be summed - consider merging them.",
              call. = FALSE)

    compound_specs <- lapply(seq_along(compound), function(i) {
      spec <- compound[[i]]
      spec$weight <- ws[i]
      spec$k      <- k
      ci <- spec$criterion
      if (!ci %in% c("D-Optimality","Ds-Optimality","A-Optimality","I-Optimality","L-Optimality"))
        stop("Unknown sub-criterion '", ci, "' in compound list.", call. = FALSE)
      if (identical(ci, "A-Optimality"))
        spec$matB <- diag(k)
      if (identical(ci, "I-Optimality") && !is.null(spec$reg_int))
        spec$matB <- integrate_reg_int(grad, k, spec$reg_int)
      if (identical(ci, "L-Optimality") && is.null(spec$matB))
        stop("L-Optimality component requires 'matB'.", call. = FALSE)
      spec
    })
  }

  # -- KL-Optimality: build kl_fun and inner-opt spec -----------------------
  kl_spec <- NULL
  if (identical(criterion, "KL-Optimality")) {
    if (!is.null(kl_fun)) {
      # User-supplied kl_fun path
      if (!is.function(kl_fun))
        stop("'kl_fun' must be a function(x, beta2).", call. = FALSE)
      if (is.null(rival_pars))
        stop("'rival_pars' must provide initial rival parameters when 'kl_fun' is used.",
             call. = FALSE)
      lower_eff <- if (is.null(rival_lower)) rep(-Inf, length(rival_pars)) else rival_lower
      upper_eff <- if (is.null(rival_upper)) rep( Inf, length(rival_pars)) else rival_upper
      kl_spec <- list(
        kl_fun     = kl_fun,
        beta2_init = rival_pars,
        lower      = lower_eff,
        upper      = upper_eff,
        kl_meta    = list(type = "kl_fun")
      )
    } else {
      # Standard family-based path: build kl_fun from model evaluators
      if (!is.character(family) || length(family) != 1L)
        stop("'family' must be a single character string for KL-Optimality.", call. = FALSE)
      if (!is.numeric(phi) || phi <= 0)
        stop("'phi' must be a positive number for KL-Optimality.", call. = FALSE)
      rival_model_eff  <- if (is.null(rival_model))  model      else rival_model
      rival_params_eff <- if (is.null(rival_params)) parameters else rival_params
      rival_pars_eff   <- if (is.null(rival_pars))   par_values else rival_pars
      lower_eff <- if (is.null(rival_lower)) rep(-Inf, length(rival_params_eff)) else rival_lower
      upper_eff <- if (is.null(rival_upper)) rep( Inf, length(rival_params_eff)) else rival_upper
      fam           <- make_glm_family(family)
      mu1_fn        <- .make_mu_eval(model, parameters, par_values)
      rival_eval_fn <- .make_rival_eval(rival_model_eff, rival_params_eff)
      kl_fun_built  <- local({
        mu1_ <- mu1_fn; riv_ <- rival_eval_fn; fam_ <- fam; phi_ <- phi
        function(x, beta2) .kl_at_point(x, mu1_, riv_, beta2, fam_, phi_)
      })
      kl_spec <- list(
        kl_fun     = kl_fun_built,
        beta2_init = rival_pars_eff,
        lower      = lower_eff,
        upper      = upper_eff,
        kl_meta    = list(type   = "family",
                          family = fam,
                          phi    = phi)
      )
    }
  }

  # -- Run cocktail algorithm ------------------------------------------------
  output <- WFMult(init_design, grad, criterion,
                   par_int = par_int, matB,
                   design_space, 1000L,
                   join_thresh, delete_thresh, k, delta, tol, tol2, max_iter,
                   compound = compound_specs,
                   kl_spec  = kl_spec)

  attr(output, "model")      <- model
  attr(output, "weight_fun") <- weight_fun
  output
}
