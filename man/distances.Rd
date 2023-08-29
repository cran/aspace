\name{distances}
\alias{distances}
\title{Multiple Euclidean distance calculator}
\description{Compute distances from a source location (point) to a series of destination locations (points).}
\usage{distances(centre.xy = NULL, destmat = NULL, verbose = FALSE)}
\arguments{
  \item{centre.xy}{Two-element vector containing x,y coordinates of the source location}
  \item{destmat}{Two-column matrix or data frame containing x,y coordinates of the activity locations}
  \item{verbose}{Boolean: Set to T if verbose output is desired}
}
\details{Distance computations are strictly Euclidean between the source point and each destination point.}
\value{A vector of distances, where each element corresponds to one of the distance between the source point and a destination (one row) from the destinations matrix.}
\author{Tarmo K. Remmel}
\note{The order of distances in the output vector corresponds to the order of destination points in the destinations object starting at row = 1 through row = n.}
\examples{
  data(centre)
  data(activities)
  distances(centre.xy=centre, destmat=activities, verbose=FALSE)  
}
\keyword{arith}
