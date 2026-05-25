# KL-Optimality -----------------------------------------------------------
# KL divergence for exponential-family GLMs:
#   KL(x, mu1, mu2) = (1/phi) * [b(t2) - b(t1) - (t2 - t1) * b'(t1)]
# where t = link(mu) is the canonical parameter.
# The inner optimisation finds rival parameters that MINIMISE KL (adversarial).
# The cocktail algorithm finds the design that MAXIMISES KL.


# Build closure: x_val -> mu  (reference model evaluated at nominal params)
.make_mu_eval <- function(model, char_vars, par_values) {
  design_vars <- detect_design_vars(model, char_vars)
  ext_vars    <- c(char_vars, design_vars)
  arglist     <- lapply(ext_vars, function(v) NULL)
  f  <- as.function(append(stats::setNames(arglist, ext_vars), quote({})))
  f1 <- stats::deriv(model, char_vars, f)
  fn <- function(x_val) as.numeric(do.call(f1, as.list(c(par_values, x_val))))
  attr(fn, "design_vars") <- design_vars
  fn
}


# Build closure: (x_val, beta2_val) -> mu  (rival model, parameterised)
.make_rival_eval <- function(rival_model, rival_params) {
  design_vars <- detect_design_vars(rival_model, rival_params)
  ext_vars    <- c(rival_params, design_vars)
  arglist     <- lapply(ext_vars, function(v) NULL)
  f  <- as.function(append(stats::setNames(arglist, ext_vars), quote({})))
  f1 <- stats::deriv(rival_model, rival_params, f)
  # Filter x_val to only the design vars the rival model uses (supports
  # rivals that ignore some of the design dimensions).
  function(x_val, beta2_val) {
    x_use <- if (!is.null(names(x_val))) x_val[design_vars] else x_val
    as.numeric(do.call(f1, as.list(c(beta2_val, x_use))))
  }
}


# KL divergence at a single point using the exponential-family cumulant formula.
.kl_at_point <- function(x, mu1_fn, rival_eval_fn, beta2_val, family, phi) {
  mu1 <- mu1_fn(x)
  mu2 <- rival_eval_fn(x, beta2_val)
  t1  <- family$link(mu1)
  t2  <- family$link(mu2)
  (family$b(t2) - family$b(t1) - (t2 - t1) * family$b_prime(t1)) / phi
}


# Integrated KL over a design given kl_fun(x, beta2).
.kl_integrated <- function(design, kl_fun, beta2) {
  dc <- coord_cols(design)
  sum(design$Weight * vapply(seq_len(nrow(design)), function(j) {
    x_j <- if (identical(dc, "Point")) design$Point[[j]]
            else stats::setNames(unlist(design[j, dc]), dc)
    kl_fun(x_j, beta2)
  }, numeric(1L)))
}


# Inner optimisation: find beta2 minimising integrated KL (adversarial rival).
.kl_minopt <- function(design, kl_fun, beta2_init, lower = -Inf, upper = Inf) {
  obj     <- function(b2) .kl_integrated(design, kl_fun, b2)
  bounded <- !all(is.infinite(lower)) || !all(is.infinite(upper))
  method  <- if (bounded) "L-BFGS-B" else "BFGS"
  res <- tryCatch(
    stats::optim(beta2_init, obj, method = method, lower = lower, upper = upper),
    error = function(e) stats::optim(beta2_init, obj, method = "Nelder-Mead")
  )
  list(beta2_star = res$par, kl_val = res$value)
}


#' Cocktail Algorithm for KL-Optimality
#'
#' @inherit WFMult return params
#' @param kl_fun function(x, beta2); returns the KL divergence at design point
#'   \code{x} given rival parameters \code{beta2}.
#' @param beta2_init numeric vector of initial rival parameter values.
#' @param lower lower bounds for rival parameters in the inner optimisation.
#' @param upper upper bounds for rival parameters in the inner optimisation.
#' @param kl_meta list with summary metadata (\code{type}, and optionally
#'   \code{family} and \code{phi} for the standard path).
#' @family cocktail algorithms
KLWFMult <- function(init_design, kl_fun, beta2_init, lower, upper,
                     design_space, grid.length,
                     join_thresh, delete_thresh,
                     delta_weights, tol, tol2, max_iter,
                     kl_meta = list(type = "kl_fun")) {
  dc        <- coord_cols(init_design)
  multi     <- is_multifactor(dc)
  maxiter_weights <- 100L
  crit_val  <- numeric(max_iter * (maxiter_weights + 1L) + 1L)
  index     <- 1L
  beta2_cur <- beta2_init
  cli::cli_progress_bar("Calculating optimal design")

  for (i in seq_len(max_iter)) {
    cli::cli_progress_update()

    inner     <- .kl_minopt(init_design, kl_fun, beta2_cur, lower, upper)
    beta2_cur <- inner$beta2_star
    crit_val[index] <- inner$kl_val
    index <- index + 1L

    sensKL <- local({ b2 <- beta2_cur; function(x) kl_fun(x, b2) })
    xmax   <- findmax(sensKL, design_space, grid.length)

    if ((as.numeric(sensKL(xmax)) - crit_val[index - 1L]) < tol2) {
      message("\n", crayon::blue(cli::symbol$info),
              " Stop condition reached: difference between sensitivity and criterion < ", tol2)
      break
    }
    init_design <- update_design(init_design, xmax, join_thresh, 1 / (index + 2))
    iter <- 1L; stopw <- FALSE
    while (!stopw) {
      weightsInit <- init_design$Weight
      inner     <- .kl_minopt(init_design, kl_fun, beta2_cur, lower, upper)
      beta2_cur <- inner$beta2_star
      crit      <- inner$kl_val
      crit_val[index] <- crit
      index <- index + 1L
      sensKL <- local({ b2 <- beta2_cur; function(x) kl_fun(x, b2) })
      init_design$Weight <- update_weightsI(init_design, sensKL, crit, delta_weights)
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

  inner     <- .kl_minopt(init_design, kl_fun, beta2_cur, lower, upper)
  beta2_cur <- inner$beta2_star
  crit_val[index] <- inner$kl_val
  crit_val <- crit_val[1L:(length(crit_val) - sum(crit_val == 0))]
  conv_plot <- plot_convergence(data.frame("criteria" = crit_val,
                                           "step"     = seq_along(crit_val)))

  init_design <- init_design[order(init_design[[dc[1L]]]), ]
  rownames(init_design) <- NULL

  sensKL        <- local({ b2 <- beta2_cur; function(x) kl_fun(x, b2) })
  xmax          <- findmax(sensKL, design_space, grid.length * 10L)
  kl_crit_final <- crit_val[length(crit_val)]
  atwood        <- kl_crit_final / as.numeric(sensKL(xmax)) * 100
  check_atwood(atwood)
  message(crayon::blue(cli::symbol$info), " The lower bound for efficiency is ", atwood, "%")

  plot_opt <- .make_sens_plot(multi, design_space, sensKL, dc, init_design, kl_crit_final)

  grad_stub <- function(x) NULL
  attr(grad_stub, "design_vars") <- dc

  l_return <- list("optdes"      = init_design,
                   "convergence" = conv_plot,
                   "sens"        = plot_opt,
                   "criterion"   = "KL-Optimality",
                   "crit_value"  = kl_crit_final,
                   "atwood"      = atwood)
  attr(l_return, "hidden_value") <- c(
    kl_meta,
    list(kl_fun     = kl_fun,
         beta2_star = beta2_cur,
         lower      = lower,
         upper      = upper)
  )
  attr(l_return, "gradient")      <- grad_stub
  attr(l_return, "design_space")  <- design_space
  attr(l_return, "crit_function") <- local({
    kl_fn_ <- kl_fun; b2_ <- beta2_cur; lo_ <- lower; up_ <- upper
    function(design) .kl_minopt(design, kl_fn_, b2_, lo_, up_)$kl_val
  })
  class(l_return) <- "optdes"
  l_return
}


#' Build a KL-divergence point function for use with opt_des()
#'
#' @description
#' Returns a \code{function(x, beta2)} computing the point KL divergence at
#' design point \code{x} given rival parameters \code{beta2}.  The result is
#' passed to \code{\link{opt_des}} via the \code{kl_fun} argument, allowing
#' discrimination between models with different family, dispersion, or mean
#' structure without having to derive the formula manually.
#'
#' Supported family pairs:
#' \itemize{
#'   \item Same family and same \code{phi}: any of
#'     \code{"Normal"}, \code{"Poisson"}, \code{"Binomial"}, \code{"Gamma"}.
#'     Uses the standard exponential-family cumulant formula.
#'   \item \code{"Normal"} vs \code{"Normal"} with \code{phi1 != phi2}:
#'     \eqn{KL = \frac{1}{2}\bigl[\log(\phi_2/\phi_1) + \phi_1/\phi_2 + (\mu_1-\mu_2)^2/\phi_2 - 1\bigr]}.
#'   \item \code{"Gamma"} vs \code{"Gamma"} with different shape
#'     (\eqn{k_i = 1/\phi_i}): closed form involving \code{digamma} and
#'     \code{lgamma}.
#' }
#' For other cross-family pairs provide \code{kl_fun} directly.
#'
#' @param family1 character; reference distribution (\code{"Normal"},
#'   \code{"Poisson"}, \code{"Binomial"} or \code{"Gamma"}).
#' @param model1 formula; reference model mean function.
#' @param params1 character vector; parameter names in \code{model1}.
#' @param par_values1 numeric vector; nominal values for the reference parameters.
#' @param family2 character; rival distribution (default: same as \code{family1}).
#' @param model2 formula; rival model mean function (default: same as \code{model1}).
#' @param params2 character vector; rival parameter names (optimised internally).
#'   Default: same as \code{params1}.
#' @param phi1 positive numeric; dispersion of the reference
#'   (\code{Normal}: variance; \code{Gamma}: \eqn{1/\text{shape}}).
#' @param phi2 positive numeric; dispersion of the rival (default: same as \code{phi1}).
#'
#' @return A function \code{function(x, beta2)} giving the point KL divergence.
#'   Works for both 1-D (\code{x} scalar) and multi-factor designs (\code{x}
#'   named numeric vector).
#' @export
#'
#' @examples
#' # Same family (Normal), different model structures
#' kl_fn <- make_kl_fun(
#'   "Normal",
#'   model1 = y ~ Vmax * x / (Km + x), params1 = c("Vmax", "Km"),
#'   par_values1 = c(2, 1),
#'   model2 = y ~ a * x, params2 = "a"
#' )
#' kl_fn(x = 1, beta2 = 0.5)
#'
#' \donttest{
#' # Normal vs Normal with different variance (phi2 = 4)
#' kl_fn2 <- make_kl_fun(
#'   "Normal",
#'   model1 = y ~ a * exp(-b * x), params1 = c("a", "b"),
#'   par_values1 = c(1, 0.5), phi1 = 1,
#'   family2 = "Normal",
#'   model2 = y ~ c * exp(-d * x), params2 = c("c", "d"), phi2 = 4
#' )
#' opt_des("KL-Optimality",
#'         model = y ~ a * exp(-b * x), parameters = c("a", "b"),
#'         par_values = c(1, 0.5), design_space = c(0, 4),
#'         kl_fun = kl_fn2, rival_pars = c(1, 1),
#'         rival_lower = c(0.5, 0.8), rival_upper = c(2, 1.5))
#' }
make_kl_fun <- function(family1, model1, params1, par_values1,
                        family2     = family1,
                        model2      = model1,
                        params2     = params1,
                        phi1        = 1,
                        phi2        = phi1) {

  if (!is.character(family1) || length(family1) != 1L)
    stop("'family1' must be a single character string.", call. = FALSE)
  if (!is.character(family2) || length(family2) != 1L)
    stop("'family2' must be a single character string.", call. = FALSE)
  if (!is.numeric(phi1) || phi1 <= 0)
    stop("'phi1' must be a positive number.", call. = FALSE)
  if (!is.numeric(phi2) || phi2 <= 0)
    stop("'phi2' must be a positive number.", call. = FALSE)

  mu1_fn        <- .make_mu_eval(model1, params1, par_values1)
  rival_eval_fn <- .make_rival_eval(model2, params2)

  same_fam <- identical(family1, family2)
  same_phi <- isTRUE(all.equal(phi1, phi2))

  if (same_fam && same_phi) {
    fam   <- make_glm_family(family1)
    phi1_ <- phi1
    return(function(x, beta2)
      .kl_at_point(x, mu1_fn, rival_eval_fn, beta2, fam, phi1_))
  }

  if (identical(family1, "Normal") && identical(family2, "Normal")) {
    # KL(N(mu1,phi1) || N(mu2,phi2)) = 0.5*(log(phi2/phi1) + phi1/phi2 + (mu1-mu2)^2/phi2 - 1)
    phi1_ <- phi1; phi2_ <- phi2
    return(function(x, beta2) {
      mu1 <- as.numeric(mu1_fn(x))
      mu2 <- as.numeric(rival_eval_fn(x, beta2))
      0.5 * (log(phi2_/phi1_) + phi1_/phi2_ + (mu1 - mu2)^2 / phi2_ - 1)
    })
  }

  if (identical(family1, "Gamma") && identical(family2, "Gamma")) {
    # KL(Gamma(mu1,k1) || Gamma(mu2,k2)), ki = 1/phi_i
    # = k2*log(k1/k2) + k2*log(mu2/mu1) + (k1-k2)*digamma(k1)
    #   - k1 + k2*mu1/mu2 - lgamma(k1) + lgamma(k2)
    k1    <- 1 / phi1; k2 <- 1 / phi2
    const <- k2 * log(k1/k2) + (k1 - k2) * digamma(k1) - k1 - lgamma(k1) + lgamma(k2)
    return(function(x, beta2) {
      mu1 <- as.numeric(mu1_fn(x))
      mu2 <- as.numeric(rival_eval_fn(x, beta2))
      const + k2 * log(mu2/mu1) + k2 * mu1/mu2
    })
  }

  stop("Family pair ('", family1, "', '", family2, "') is not supported by ",
       "make_kl_fun(). Provide kl_fun directly to opt_des().", call. = FALSE)
}
