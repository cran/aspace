\name{calc_sdd}
\alias{calc_sdd}
\title{Calculate the Standard Distance Deviation (Standard Distance)}
\description{This function computes the Standard Distance Deviation (SDD) or Standard Distance from a set of points.}
\usage{calc_sdd(id=1, centre.xy=NULL, calccentre=TRUE, weighted=FALSE,
 weights=NULL, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify a SDD estimate}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDD centre}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations input to the calc_sdd function}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{Use the LOCATIONS element in the output list object along with the ATTRIBUTES elements can be used to produce shapefiles or other vector point files for geographic data.}
\value{
   The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CF, or CF2PTS}
  \item{DATE}{The date and time that the function was run}
  \item{LOCATIONS}{Locations pertinent for the BOX that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting by plot_sdd()}
  \item{ATTRIBUTES}{Attributes for the output BOX that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
    \item{id}{Identifier for the SDD shape - it should be unique}
    \item{calccentre}{Boolean: TRUE if mean centre is computed}
    \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
    \item{CENTRE.x}{X-coordinate of the centre}
    \item{CENTRE.y}{Y-coordinate of the centre}
    \item{SDD.radius}{SDD value, radius of the SDD}
    \item{SDD.area}{Area of the SDD circle}
}
\author{Tarmo K. Remmel, Randy Bui, Ron Buliung}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. Pass the entire object to plot_sdd() and the function automatically extracts this information. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each SDD has a unique identifier.}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{calc_sde}}, 
  \code{\link{calc_box}}
}
\examples{
# SDD EXAMPLE
data(activities)
a <- calc_sdd(id=1, centre.xy=NULL, calccentre=TRUE, points=activities)
str(a)
print(a)

# IF THE RESULT OF THIS FUNCTION IS STORED TO AN OBJECT, THE plot_box()
# FUNCTION WILL TAKE THAT OBJECT AS INPUT FOR PLOTTING VIA THE datin ARGUMENT

# SDD TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES,"id",5)
# write.shapefile(shp, "SDD_Shape", arcgis=T)
}

\keyword{arith}
