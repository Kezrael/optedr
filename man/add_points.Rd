% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment.R
\name{add_points}
\alias{add_points}
\title{Update design given crosspoints and alpha}
\usage{
add_points(points, alpha, design)
}
\arguments{
\item{points}{Points to be added to the design}

\item{alpha}{Combined weight of the new points to be added to the design}

\item{design}{A design as a dataframe with "Point" and "Weight" columns}
}
\value{
A design as a dataframe with "Point" and "Weight" columns that is the
addition of the design and the new points
}
\description{
Given a set of points, a weight and the design, the function adds these points
to the new design with uniform weight, and combined weight alpha
}
