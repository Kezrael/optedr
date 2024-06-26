% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficient_round.R
\name{combinatorial_round}
\alias{combinatorial_round}
\title{Combinatorial round}
\usage{
combinatorial_round(
  design,
  n,
  criterion = NULL,
  model = NULL,
  parameters = NULL,
  par_values = NULL,
  weight_fun = function(x) 1,
  par_int = NULL,
  reg_int = NULL,
  matB = NULL
)
}
\arguments{
\item{design}{either a dataframe with the design to round, or an object of class "optdes". If the former,
the criterion, model and parameters must be specified. The dataframe should have two columns:
\itemize{
\item \code{Point} contains the support points of the design.
\item \code{Weight} contains the corresponding weights of the \code{Point}s.
}}

\item{n}{integer with the desired number of points of the resulting design.}

\item{criterion}{character variable with the chosen optimality criterion. Can be one of the following:
\itemize{
\item 'D-Optimality'
\item 'Ds-Optimality'
\item 'A-Optimality'
\item 'I-Optimality'
\item 'L-Optimality'
}}

\item{model}{formula describing the model. Must use x as the variable.}

\item{parameters}{character vector with the parameters of the models, as written in the \code{formula}.}

\item{par_values}{numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.}

\item{weight_fun}{optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response.}

\item{par_int}{optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.}

\item{matB}{optional matrix of dimensions k x k, for L-optimality.}
}
\value{
A data.frame with the rounded design to n number of points
}
\description{
Given an approximate design and a number of points, computes all the possible combinations of
roundings of each point to the nearest integer, keeps the ones that amount to the requested number of points,
and returns the one with the best value for the criterion function
}
\examples{
aprox_design <- opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
combinatorial_round(aprox_design, 27)
}
