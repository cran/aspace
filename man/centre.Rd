\name{centre}
\alias{centre}
\docType{data}
\title{Demo Data: Coordinates of a single source, centre, location}
\description{This is a simple two-element vector containing x,y coordinates for a source or central location associated with a spatial point pattern. In this example, the center location represents a point of importance in an individuals daily activity pattern. Surrounding point locations are places physically contacted by an individual during a particular time interval. Demonstration data mimics UTM coordinates such that the first element represents Easting (x), and the second, Northing (y).}
\usage{data(centre)}
\format{The format is a two-element vector of numeric entries.}
\details{The coordinates of the center must have the same units and projection as the remaining point observations.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(centre)
  str(centre)
  plot(centre)
}
\keyword{datasets}
