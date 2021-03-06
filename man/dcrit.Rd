% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crit_eff.R
\name{dcrit}
\alias{dcrit}
\title{Criterion function for D-Optimality}
\usage{
dcrit(M, k)
}
\arguments{
\item{M}{Information matrix for which the criterion value wants to be calculated.}

\item{k}{Numeric number of parameters of the model. Taken from the number of rows of the matrix if omitted.}
}
\value{
numeric value of the D-optimality criterion for the information matrix.
}
\description{
Calculates the value of the D-Optimality criterion, which follows the expression:
\deqn{\phi_D = \frac{1}{|M|}^{1/k}}
}
\examples{
optedr:::dcrit(matrix(c(1, 0.75, 0.75, 0.625), nrow = 2), k = 2)
}
