\name{calc_cf2pts}
\alias{calc_cf2pts}
\title{Central feature between 2 point patterns (CF2PTS) Calculator}
\description{Central feature of point2 within point1. Identifies the central feature as the point location in the first pattern that has the smallest cummulative distance to features in a second point pattern.}
\usage{calc_cf2pts(id=1, points1=NULL, points2=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the CF2PTS}
  \item{points1}{A 2-column matrix or data frame containing the first set of point observations}
  \item{points2}{A 2-column matrix or data frame containing the second set of point observations}
  \item{verbose}{A Boolean flag to control verbose reporting on monitor}
}
\details{Use the LOCATIONS element in the output list object along with the ATTRIBUTES elements can be used to produce shapefiles or other vector point files for geographic data.}
\value{
  The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the central feature - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the CF2PTS that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting.}
  \item{ATTRIBUTES}{Attributes for the output CF2PTS that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}

    \item{id}{Identifier for the central feature - it should be unique}
    \item{CF2PTS.x}{X-coordinate of the central feature}
    \item{CF2PTS.y}{Y-coordinate of the central feature}
  }
\author{Randy Bui, Ron Buliung}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each CF2PTS has a unique identifier.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{calc_cmd}},
  \code{\link{calc_cf}},
  \code{\link{calc_mnc}},
  \code{\link{calc_mdc}}
}
\examples{
# CF2PTS EXAMPLE
data(activities)
data(activities2)
a <- calc_cf2pts(id=1, points1=activities, points2=activities2)
str(a)
print(a)

# IF THE RESULT OF THIS FUNCTION IS STORED TO AN OBJECT, THE plot_box()
# FUNCTION WILL TAKE THAT OBJECT AS INPUT FOR PLOTTING VIA THE datin ARGUMENT

# CF2PTS TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "CF2PTS_Shape", arcgis=T)
}
\keyword{arith}
