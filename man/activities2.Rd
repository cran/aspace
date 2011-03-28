\name{activities2}
\alias{activities2}
\docType{data}
\title{Demo Data: x and y coordinates of 10 specified point locations}
\description{This is a simple two-column data frame (or matrix) containing x,y coordinates for a series of point locations. These data mimic UTM coordinates such that the first column contains Easting (x), and the second Northing (y) coordinates for the set of unique points.}
\usage{data(activities2)}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{col1}}{A numeric vector of x-coordinates}
    \item{\code{col2}}{A numeric vector of y-coordinates}
  }
}
\details{The coordinates of the points must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(activities2)
  str(activities2)
  plot(activities2)
}
\keyword{datasets}
