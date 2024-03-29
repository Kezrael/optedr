% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crit_eff.R
\name{dscrit}
\alias{dscrit}
\title{Criterion function for Ds-Optimality}
\usage{
dscrit(M, par_int)
}
\arguments{
\item{M}{information matrix for which the criterion value wants to be calculated.}

\item{par_int}{numeric vector with the index of the parameters of interest of the model.}
}
\value{
Numeric value of the Ds-optimality criterion for the information matrix.
}
\description{
Calculates the value of the Ds-Optimality criterion function, which follows the expression:
\deqn{\phi_{Ds} = \left(\frac{|M_{22}|}{|M|}\right)^{1/s}}
}
