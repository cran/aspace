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
\details{Use the medianloc (coordinates) and medianatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the median centre - it should be unique}
  \item{median.x}{X-coordinate of the median centre}
  \item{median.y}{Y-coordinate of the median centre}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.median object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple median centres from different datasets.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{CMD}},
  \code{\link{CF}}
}
\examples{
## Median centre example
median_centre(id=1, filename="median_centre_Output.txt", 
points=activities)

## Median centre to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(medianloc,medianatt,"id",5)
## write.shapefile(shp, "Median_Shape", arcgis=T)
}
\keyword{arith}
