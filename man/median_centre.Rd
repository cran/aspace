\name{median_centre}
\alias{median_centre}
\title{Median Centre Calculator}
\description{Compute the median centre from a series of point locations.}
\usage{median_centre(id=1, filename="median_centre_Output.txt", 
points=activities)}
\arguments{
  \item{id}{A unique integer to identify the median centre}
  \item{filename}{A string indicating the ASCII textfile where centre coordinates will be written}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{This function is most powerful when used repetitively within a loop to compute multiple median centres from different datasets.}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the median centre - it should be unique}
  \item{median.x}{X-coordinate of the median centre}
  \item{median.y}{Y-coordinate of the median centre}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.median object and can be passed through plotting functions.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{CMD}},
  \code{\link{CF}}
}
\examples{median_centre(id=1, filename="median_centre_Output.txt", 
points=activities)}
\keyword{arith}
