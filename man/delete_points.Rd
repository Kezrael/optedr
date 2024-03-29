% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{delete_points}
\alias{delete_points}
\title{Remove low weight points}
\usage{
delete_points(design, delta)
}
\arguments{
\item{design}{The design from which to remove points as a dataframe with two columns:
\itemize{
\item \code{Point} contains the support points of the design.
\item \code{Weight} contains the corresponding weights of the \code{Point}s.
}}

\item{delta}{The threshold from which the points with such a weight or lower will be removed.}
}
\value{
The design without the removed points.
}
\description{
Removes the points of a design with a weight lower than a threshold, \code{delta}, and distributes that weights
proportionally to the rest of the points.
}
