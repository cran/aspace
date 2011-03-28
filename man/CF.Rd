\name{CF}
\alias{CF}
\title{Central Feature (CF) Calculator}
\description{Identifies the central feature within a set of point locations.}
\usage{CF(id=1, filename="CF_Output.txt", points=activities)}
\arguments{
  \item{id}{A unique integer to identify the CF}
  \item{filename}{A string indicating the ASCII textfile where the central feature coordinates will be written}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{Use the cfloc (coordinates) and cfdatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the central feature - it should be unique}
  \item{CF.x}{X-coordinate of the central feature}
  \item{CF.y}{Y-coordinate of the central feature}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.CF object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple CF centres from different datasets.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{CMD}},
  \code{\link{median_centre}}
}
\examples{
## CF example
CF(id=1, filename="CF_Output.txt", points=activities)

## CF to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(cfloc,cfatt,"id",5)
## write.shapefile(shp, "CF_Shape", arcgis=T)
}
\keyword{arith}