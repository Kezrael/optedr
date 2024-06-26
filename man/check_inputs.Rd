% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_checks.R
\name{check_inputs}
\alias{check_inputs}
\title{Check Inputs}
\usage{
check_inputs(
  criterion,
  model,
  parameters,
  par_values,
  design_space,
  init_design,
  join_thresh,
  delete_thresh,
  delta,
  tol,
  tol2,
  par_int,
  matB,
  reg_int,
  desired_output,
  weight_fun
)
}
\arguments{
\item{criterion}{character variable with the chosen optimality criterion. Can be one of the following:
\itemize{
\item 'D-Optimality'
\item 'Ds-Optimality'
\item 'A-Optimality'
\item 'I-Optimality'
\item 'L-Optimality'
}}

\item{model}{formula describing the model to calculate the optimal design. Must use x as the variable.}

\item{parameters}{character vector with the parameters of the models, as written in the \code{formula}.}

\item{par_values}{numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.}

\item{design_space}{numeric vector with the limits of the space of the design.}

\item{init_design}{optional dataframe with the initial design for the algorithm. A dataframe with two columns:
\itemize{
\item \code{Point} contains the support points of the design.
\item \code{Weight} contains the corresponding weights of the \code{Point}s.
}}

\item{join_thresh}{optional numeric value that states how close, in real units, two points must be in order to
be joined together by the join heuristic.}

\item{delete_thresh}{optional numeric value with the minimum weight, over 1 total, that a point needs to have
in order to not be deleted from the design.}

\item{delta}{optional numeric value in (0, 1), parameter of the algorithm.}

\item{tol}{optional numeric value for the convergence of the weight optimizing algorithm.}

\item{tol2}{optional numeric value for the stop criterion: difference between maximum of sensitivity function
and optimality criterion.}

\item{par_int}{optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.}

\item{matB}{optional matrix of dimensions k x k, for L-optimality.}

\item{reg_int}{optional numeric vector of two components with the bounds of the interest region for I-Optimality.}

\item{desired_output}{not functional yet: decide which kind of output you want.}

\item{weight_fun}{optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response.}
}
\description{
Function to check that the inputs given to the function \code{opt_des} are correct. If not, throws the
correspondent error message.
}
