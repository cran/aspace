\name{CF}
\alias{CF}
\title{Central Feature (CF) Calculator}
\description{Identifies the central feature from a series of point locations.}
\usage{CF(id=1, filename="CF_Output.txt", points=activities)}
\arguments{
  \item{id}{A unique integer to identify the CF}
  \item{filename}{A string indicating the ASCII textfile where the central feature coordinates will be written}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{This function is most powerful when used repetitively within a loop to compute multiple CF from different datasets.}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the central feature - it should be unique}
  \item{CF.x}{X-coordinate of the central feature}
  \item{CF.y}{Y-coordinate of the central feature}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.CF object and can be passed through plotting functions.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{CMD}},
  \code{\link{median_centre}}
}
\examples{CF(id=1, filename="CF_Output.txt", points=activities)}
\keyword{arith}