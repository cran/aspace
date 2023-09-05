\name{calc_mdc}
\alias{calc_mdc}
\title{Median Centre Calculator}
\description{Compute the median centre from a series of point locations.}
\usage{calc_mdc(id=1, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the median centre}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
  \item{verbose}{A Boolean flag to control verbose content on the monitor}
}
\details{Use the medianloc (coordinates) and medianatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
    The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the median centre - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the MDC that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting.}
  \item{ATTRIBUTES}{Attributes for the output MDC that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
  \item{id}{Identifier for the median centre - it should be unique}
  \item{median.x}{X-coordinate of the median centre}
  \item{median.y}{Y-coordinate of the median centre}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.median object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple median centres from different datasets.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{calc_cmd}},
  \code{\link{calc_cf}},
  \code{\link{calc_cf2pts}},
  \code{\link{calc_mnc}}
}
\examples{
# MEDIAN CENTRE EXAMPLE
a <- calc_mdc(id=1, points=activities)
str(a)
print(a)

# MEDIAN CENTRE TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "Median_Shape", arcgis=T)
}
\keyword{arith}
