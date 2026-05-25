# GLM family objects for KL-Optimality ----------------------------
#
# Each family is a list with:
#   b(theta)        cumulant generating function
#   b_prime(theta)  mean: mu = b'(theta)
#   b_dbl(theta)    variance function: V(mu) = phi * b''(theta)
#   link(mu)        canonical link: theta = link(mu)
#   link_inv(eta)   inverse canonical link: mu = link_inv(eta)
#   name            character identifier
#
# The model formula in opt_des() gives the MEAN mu(x, theta) directly.
# KL-optimality converts mu -> canonical theta via link(), then applies
# the cumulant-based KL formula.


#' GLM family specification for KL-Optimality
#'
#' @description
#' Returns a GLM family object for use with \code{criterion = "KL-Optimality"}.
#' The family encodes the cumulant generating function and canonical link needed
#' to compute the Kullback-Leibler divergence between two distributions.
#'
#' @param name character; one of \code{"Normal"}, \code{"Poisson"},
#'   \code{"Binomial"} or \code{"Gamma"}.
#'
#' @return A named list with elements \code{b}, \code{b_prime}, \code{b_dbl},
#'   \code{link}, \code{link_inv} and \code{name}.
#' @export
#'
#' @examples
#' make_glm_family("Poisson")
#' make_glm_family("Normal")
make_glm_family <- function(name) {
  switch(name,
    Normal = list(
      name     = "Normal",
      b        = function(theta) theta^2 / 2,
      b_prime  = function(theta) theta,               # mu = theta
      b_dbl    = function(theta) rep(1, length(theta)),
      link     = function(mu) mu,                     # identity (canonical)
      link_inv = function(eta) eta
    ),
    Poisson = list(
      name     = "Poisson",
      b        = function(theta) exp(theta),
      b_prime  = function(theta) exp(theta),          # mu = exp(theta)
      b_dbl    = function(theta) exp(theta),          # V(mu) = mu
      link     = function(mu) log(pmax(mu, .Machine$double.eps)),
      link_inv = function(eta) exp(eta)
    ),
    Binomial = list(
      name     = "Binomial",
      b        = function(theta) log(1 + exp(theta)),
      b_prime  = function(theta) {
        e <- exp(theta); e / (1 + e)                  # mu = logistic(theta)
      },
      b_dbl    = function(theta) {
        p <- exp(theta) / (1 + exp(theta)); p * (1 - p)
      },
      link     = function(mu) {
        mu <- pmax(pmin(mu, 1 - .Machine$double.eps), .Machine$double.eps)
        log(mu / (1 - mu))                            # logit (canonical)
      },
      link_inv = function(eta) exp(eta) / (1 + exp(eta))
    ),
    Gamma = list(
      name     = "Gamma",
      b        = function(theta) -log(-theta),        # theta < 0
      b_prime  = function(theta) -1 / theta,          # mu = -1/theta > 0
      b_dbl    = function(theta) 1 / theta^2,         # V(mu) = mu^2
      link     = function(mu) -1 / pmax(mu, .Machine$double.eps),
      link_inv = function(eta) -1 / eta               # eta must be < 0
    ),
    stop("Unknown GLM family '", name,
         "'. Choose from: Normal, Poisson, Binomial, Gamma.", call. = FALSE)
  )
}
