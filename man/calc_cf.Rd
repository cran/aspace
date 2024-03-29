\name{calc_cf}
\alias{calc_cf}
\title{Central Feature (CF) Calculator}
\description{Identifies the central feature within a set of point locations.}
\usage{calc_cf(id=1, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the CF}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
  \item{verbose}{Boolean flag for verbose output to monitor}
}
\details{Use the LOCATIONS element in the output list object along with the ATTRIBUTES elements can be used to produce shapefiles or other vector point files for geographic data.}
\value{
  The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the central feature - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the CF that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting.}
  \item{ATTRIBUTES}{Attributes for the output CF that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
    \item{id}{Identifier for the central feature - it should be unique}
    \item{CF.x}{X-coordinate of the central feature}
    \item{CF.y}{Y-coordinate of the central feature}
  }
\author{Randy Bui, Ron Buliung, Tarmo K Remmel}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. Pass the entire object to plot_box() and the function automatically extracts this information. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each CF has a unique identifier.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{calc_cmd}},
  \code{\link{calc_cf2pts}},
  \code{\link{calc_mnc}},
  \code{\link{calc_mdc}}
}
\examples{
# CF EXAMPLE
data(activities)
a <- calc_cf(id=1, points=activities)
str(a)
print(a)

# BOX TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "CF_Shape", arcgis=T)

}
\keyword{arith}
