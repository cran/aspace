\name{calc_cmd}
\alias{calc_cmd}
\title{Centre of Minimum Distance (CMD) Calculator}
\description{Compute the CMD within a set of point locations.}
\usage{calc_cmd(id=1, dist=100, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the CMD}
  \item{dist}{Hold distance value between i and ith iterations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
  \item{verbose}{A Boolean flag to control verbose feedback on screen}
}
\details{Use the cmdloc (coordinates) and cmdatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the CMD - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the CMD that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting by plot_cmd()}
  \item{ATTRIBUTES}{Attributes for the output CMD that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
  \item{id}{Identifier for the CMD - it should be unique}
  \item{CMD.x}{X-coordinate of the CMD}
  \item{CMD.y}{Y-coordinate of the CMD}
  \item{distance}{Hold distance value between i and ith iterations (metres}
  \item{Number of Cells}{Hold number of cells in each grid created for each iteration}
  }
\author{Randy Bui, Ron Buliung, Tarmo K. Remmel}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. Pass the entire object to plot_box() and the function automatically extracts this information. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each SD BOX has a unique identifier.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{calc_cf}},
  \code{\link{calc_cf2pts}},
  \code{\link{calc_mnc}},
  \code{\link{calc_mdc}}
}
\examples{
# CMD EXAMPLE
a <- calc_cmd(id=1, dist=100, points=activities)
str(a)
print(a)

# CMD TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "CMD_Shape", arcgis=T)
}
\keyword{arith}
