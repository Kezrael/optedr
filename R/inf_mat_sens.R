# Gradient Vector ------------------------------------


#' Gradient function
#'
#' @description
#' Calculates the gradient function of a \code{model} with respect to the parameters, \code{char_vars}, evaluates
#' it at the provided \code{values} and returns the result as a function of the variable \code{x}.
#'
#' @param model formula describing the model, which must contain only \code{x}, the parameters defined in
#'   \code{char_vars} and the numerical operators.
#' @param char_vars character vector of the parameters of the model.
#' @param values numeric vector with the nominal values of the parameters in \code{char_vars}.
#' @param weight_fun optional function variable that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#'
#' @return A function depending on \code{x} that's the gradient of the \code{model} with respect to \code{char_vars}
gradient <- function(model, char_vars, values, weight_fun = function(x) 1) {
  design_vars   <- detect_design_vars(model, char_vars)
  ext_char_vars <- c(char_vars, design_vars)
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f  <- as.function(append(stats::setNames(arglist, ext_char_vars), quote({})))
  f1 <- stats::deriv(model, char_vars, f)
  # x_val: scalar for single-factor, or named numeric vector for multi-factor.
  # c(values, x_val) passes parameters positionally and design variables by name
  # via do.call — works for both cases without branching.
  f2 <- function(x_val) {
    attr(do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  f3 <- function(x) f2(x) * weight_fun(x)
  attr(f3, "design_vars") <- design_vars
  f3
}


# Information Matrix ------------------------------------


#' Information Matrix
#'
#' @description
#' Given the gradient vector of a model in a single variable model and a design, calculates the information matrix.
#'
#' @param grad A function in a single variable that returns the partial derivatives vector of the model.
#' @param design A dataframe that represents the design. Must have two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#'
#' @return The information matrix of the design, a \eqn{k\times k} matrix where k is the length of the gradient.
inf_mat <- function(grad, design) {
  dvars <- attr(grad, "design_vars")
  if (is.null(dvars)) dvars <- "x"          # backward compat if attr absent
  design <- normalize_design_cols(design, dvars)

  if (!is_multifactor(dvars)) {
    # ── Single-factor path (current implementation) ──────────────────────────
    k     <- length(grad(design[[dvars]][[1L]]))
    F_mat <- vapply(design[[dvars]], function(x) as.vector(grad(x)), numeric(k))
  } else {
    # ── Multi-factor path ─────────────────────────────────────────────────────
    # Evaluate gradient at each design point (a named row vector)
    x0    <- unlist(design[1L, dvars])
    k     <- length(grad(x0))
    F_mat <- vapply(seq_len(nrow(design)), function(i) {
      as.vector(grad(unlist(design[i, dvars])))
    }, numeric(k))
  }

  if (!is.matrix(F_mat)) F_mat <- matrix(F_mat, nrow = 1L)
  # M = F diag(w) F^T = crossprod(sqrt(w) * F^T)  [LAPACK DSYRK]
  crossprod(sqrt(design$Weight) * t(F_mat))
}

# Sensibility Function ------------------------------------

#' Master function to calculate the sensitivity function
#'
#' @description
#' Calculates the sensitivity function given the desired \code{Criterion}, an information matrix and other
#' necessary values depending on the chosen criterion.
#'
#' @param Criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#'   * 'L-Optimality'
#' @param grad A function in a single variable that returns the partial derivatives vector of the model.
#' @param M Information Matrix for the sensitivity function.
#' @param par_int Numeric vector of the indexes of the parameters of interest for Ds-Optimality.
#' @param matB Matrix resulting from the integration of the one-point Information Matrix along the interest
#'   region or lineal matrix for L-Optimality.
#'
#' @return The sensitivity function as a matrix of single variable.
sens <- function(Criterion, grad, M, par_int = c(1), matB = NA) {
  if (identical(Criterion, "D-Optimality")) {
    return(dsens(grad, M))
  }
  else if (identical(Criterion, "Ds-Optimality")) {
    return(dssens(grad, M, par_int))
  }
  else if (identical(Criterion, "A-Optimality")) {
    return(isens(grad, M, diag(nrow(M))))
  }
  else if (identical(Criterion, "I-Optimality") || identical(Criterion, "L-Optimality")) {
    return(isens(grad, M, matB))
  }
}


#' Sensitivity function for D-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector and the Identity Matrix.
#'
#' @inherit sens params return
#'
dsens <- function(grad, M) {
  invMat <- inv_spd(M)
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col))
  }
}


#' Sensitivity function for Ds-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector, the Identity Matrix and the parameters of
#' interest.
#'
#' @inherit sens params return
#'
dssens <- function(grad, M, par_int) {
  invMat <- inv_spd(M)
  if (length(M[-par_int, -par_int]) == 1) {
    invMat22 <- 1 / M[-par_int, -par_int]
  } else {
    invMat22 <- inv_spd(M[-par_int, -par_int])
  }
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% t(f_col) - f_col[-par_int] %*% invMat22 %*% as.matrix(f_col[-par_int], ncol = 1))
  }
}


#' Sensitivity function for I-Optimality
#'
#' @description
#' Calculates the sensitivity function from the gradient vector, the Information Matrix and the integral of the
#' one-point Identity Matrix over the interest region. If instead the identity matrix is used, it can be used
#' for A-Optimality.
#'
#' @inherit sens params return
#'
isens <- function(grad, M, matB) {
  invMat <- inv_spd(M)
  sens_ret <- function(xval) {
    f_col <- as.matrix(grad(xval), nrow = 1, ncol = 3, byrow = TRUE, dimnames = NULL)
    return(f_col %*% invMat %*% matB %*% invMat %*% t(f_col))
  }
}


# Compound sensitivity function.
# compound_specs: list of per-criterion specs (see opt_des compound parameter).
# log_scale = FALSE (default): d_c(xval) = sum_i w_i * d_i(xval).
# log_scale = TRUE: d_c(xval) = sum_i [w_i / threshold_i(M)] * d_i(xval), the directional derivative of
# sum_i w_i * log(phi_i(M)) (see ccrit()). Must stay in sync with compound_threshold(log_scale = TRUE).
csens <- function(compound_specs, grad, M, log_scale = FALSE) {
  sens_fns <- lapply(compound_specs, function(spec) {
    if (identical(spec$criterion, "D-Optimality"))
      dsens(grad, M)
    else if (identical(spec$criterion, "Ds-Optimality"))
      dssens(grad, M, spec$par_int)
    else
      isens(grad, M, spec$matB)   # A, I, L
  })
  weights <- sapply(compound_specs, `[[`, "weight")
  if (log_scale)
    weights <- weights / vapply(compound_specs, .compound_component_threshold, numeric(1L), M)
  function(xval) {
    sum(mapply(function(s, w) w * as.numeric(s(xval)), sens_fns, weights))
  }
}


# Equivalence Theorem threshold for the compound criterion.
# log_scale = FALSE (default): sum_i w_i * threshold_i, where threshold_i = k (D), s (Ds),
#   tr(B M^{-1}) (A/I/L).
# log_scale = TRUE: sum_i w_i, i.e. 1 (weights are normalised to sum to 1 by opt_des()) - the
#   threshold_i factors cancel against the same factors in csens(log_scale = TRUE).
compound_threshold <- function(compound_specs, M, log_scale = FALSE) {
  weights <- sapply(compound_specs, `[[`, "weight")
  if (log_scale) return(sum(weights))
  sum(weights * vapply(compound_specs, .compound_component_threshold, numeric(1L), M))
}


# threshold_i(M) of a single compound component: k (D), s (Ds), tr(B M^{-1}) (A/I/L) - the latter
# equals phi_i(M) itself (see .compound_component_phi() in crit_eff.R).
.compound_component_threshold <- function(spec, M) {
  if (identical(spec$criterion, "D-Optimality"))
    spec$k
  else if (identical(spec$criterion, "Ds-Optimality"))
    length(spec$par_int)
  else
    icrit(M, spec$matB)
}
