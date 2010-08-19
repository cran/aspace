\name{CMD}
\alias{CMD}
\title{Centre of Minimum Distance (CMD) Calculator}
\description{Compute the CMD from a series of point locations.}
\usage{CMD(id=1, filename="CMD_Output.txt", CMD.npts=10000, 
points=activities)}
\arguments{
  \item{id}{A unique integer to identify the CMD}
  \item{filename}{A string indicating the ASCII textfile where centre coordinates will be written}
  \item{CMD.npts}{The approximate number of grid points within the convex hull for the centre of minimum distance (CMD) calculation}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{This function is most powerful when used repetitively within a loop to compute multiple CMD centres from different datasets.}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the CMD - it should be unique}
  \item{CMD.x}{X-coordinate of the CMD}
  \item{CMD.y}{Y-coordinate of the CMD}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.CMD object and can be passed through plotting functions. As the value of 'CMD.npts' increases, the more precise the centre of minimum distance is estimated.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{median_centre}},
  \code{\link{CF}}
}
\examples{CMD(id=1, filename="CMD_Output.txt", CMD.npts=10000, 
points=activities)}
\keyword{arith}
