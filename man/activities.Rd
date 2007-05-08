\name{activities}
\alias{activities}
\docType{data}
\title{Demo Data: Coordinates of 10 specified activity locations}
\description{This is a simple two-column data frame (or matrix) containing x,y coordinates for a series of activity point locations.  These are meant to represent locations physically contacted by an individual during a specific time interval. Demonstration data mimic UTM coordinates such that the first column contains Easting (x), and the second Northing (y) coordinates for unique destinations (one destination per row).}
\usage{data(activities)}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{col1}}{A numeric vector of x-coordinates}
    \item{\code{col2}}{A numeric vector of y-coordinates}
  }
}
\details{The coordinates of the activities must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(activities)
  str(activities)
  plot(activities)
}
\keyword{datasets}
