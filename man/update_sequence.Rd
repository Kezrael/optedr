% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{update_sequence}
\alias{update_sequence}
\title{Deletes duplicates points}
\usage{
update_sequence(points, tol)
}
\arguments{
\item{points}{Points to be updated}

\item{tol}{Tolerance for which two points are considered the same}
}
\value{
The points without duplicates
}
\description{
Within a vector of points, deletes points that are close enough (less than the tol parameter).
Returns the points without the "duplicates"
}
