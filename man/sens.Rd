% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inf_mat_sens.R
\name{sens}
\alias{sens}
\title{Master function to calculate the sensitivity function}
\usage{
sens(Criterion, grad, M, par_int = c(1), matB = NA)
}
\arguments{
\item{Criterion}{Character with the chosen optimality criterion. Can be one of the following:
\itemize{
\item 'D-Optimality'
\item 'Ds-Optimality'
\item 'A-Optimality'
\item 'I-Optimality'
}}

\item{grad}{A function in a single variable that returns the partial derivatives vector of the model.}

\item{M}{Information Matrix for the sensitivity function.}

\item{par_int}{Numeric vector of the indexes of the parameters of interest for Ds-Optimality.}

\item{matB}{Matrix resulting from the integration of the unipunctual Information Matrix along the interest
region.}
}
\value{
The sensitivity function as a matrix of single variable.
}
\description{
Calculates the sensitivity function given the desired \code{Criterion}, an information matrix and other
necessary values depending on the chosen criterion.
}
\examples{
grad <- optedr:::gradient(y ~ a * exp(-b / x), c("a", "b"), c(1, 1500))
design <- data.frame("Point" = seq(212, 422, length.out = 3), "Weight" = rep(1 / 3, times = 3))
matrix <- optedr:::inf_mat(grad, design)
optedr:::sens("D-Optimality", grad, matrix)
}
