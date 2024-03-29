% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plot_sens}
\alias{plot_sens}
\title{Plot sensitivity function}
\usage{
plot_sens(min, max, sens_function, criterion_value)
}
\arguments{
\item{min}{Minimum of the space of the design, used in the limits of the representation.}

\item{max}{Maximum of the space of the design, used in the limits of the representation.}

\item{sens_function}{A single variable function, the sensitivity function.}

\item{criterion_value}{A numeric value representing the other side of the inequality of the Equivalence Theorem.}
}
\value{
A \code{ggplot} object that represents the sensitivity function
}
\description{
Plots the sensitivity function and the value of the Equivalence Theorem as an horizontal line, which helps
assess the optimality of the design of the given sensitivity function.
}
