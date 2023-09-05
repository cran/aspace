\name{calc_mnc}
\alias{calc_mnc}
\title{Mean Centre Calculator}
\description{Compute the mean centre from a series of point locations.}
\usage{calc_mnc(id=1, weighted=FALSE, weights=NULL,
 points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the mean centre}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
  \item{verbose}{A Boolean flag that controls verbose feedback to the monitor}
}
\details{Use the meanloc (coordinates) and meanatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
   The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the mean centre - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the MNC that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting.}
  \item{ATTRIBUTES}{Attributes for the output MNC that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
  \item{id}{Identifier for the mean centre - it should be unique}
  \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
  \item{weights}{Weights applied to point observations}
  \item{CENTRE.x}{X-coordinate of the mean centre}
  \item{CENTRE.y}{Y-coordinate of the mean centre}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.mean object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple mean centres from different datasets.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{calc_cmd}},
  \code{\link{calc_cf}},
  \code{\link{calc_cf2pts}},
  \code{\link{calc_mdc}}
}
\examples{
# MEAN CENTRE EXAMPLE
a <- calc_mnc(id=1, points=activities)
str(a)
print(a)

# MEAN CENTRE TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "Mean_Shape", arcgis=T)
}
\keyword{arith}
