\name{median_centre}
\alias{median_centre}
\title{Median Centre Calculator}
\description{Compute the median centre from a series of point locations.}
\usage{median_centre(id=1, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the median centre}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
  \item{verbose}{A Boolean flag to control verbose content on the monitor}
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
  \code{\link{mean_centre}},
  \code{\link{CF}}
}
\examples{
# MEDIAN CENTRE EXAMPLE
a <- median_centre(id=1, points=activities)
str(a)
print(a)

# MEDIAN CENTRE TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$medianloc, a$medianatt, "id", 5)
# write.shapefile(shp, "Median_Shape", arcgis=T)
}
\keyword{arith}
