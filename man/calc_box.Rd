\name{calc_box}
\alias{calc_box}
\title{Calculate the Standard Deviation Box}
\description{The orthogonal dispersion of a set of points can be described using the standard deviation of the x- and y-coordinates of a set of point observations. The orthogonal dispersion can then be visualized with a Standard Deviation Box. This function computes the properties of the Standard Deviation Box (SD Box) from a set of point observations.}
\usage{calc_box(id=1, centre.xy=NULL, calccentre=TRUE, weighted=FALSE,
weights=NULL, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify a SD Box}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the geographic centre of the SD Box}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations input to the calc_box function}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{Use the LOCATIONS element in the output list object along with the ATTRIBUTES elements can be used to produce shapefiles or other vector point files for geographic data.}
\value{
  The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CF, or CF2PTS}
  \item{DATE}{The date and time that the function was run}
  \item{LOCATIONS}{Locations pertinent for the BOX that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting by plot_box()}
  \item{ATTRIBUTES}{Attributes for the output BOX that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
    \item{id}{Identifier for the SD Box shape - it should be unique}
    \item{calccentre}{Boolean: TRUE if the mean centre was estimated}
    \item{weighted}{Boolean: TRUE if the weighted mean centre was estimated}
    \item{CENTRE.x}{X-coordinate of the centre}
    \item{CENTRE.y}{Y-coordinate of the centre}
    \item{SD.x}{Orthogonal standard deviation in the x-axis}
    \item{SD.y}{Orthogonal standard deviation in the y-axis}
    \item{Box.area}{Area of the standard deviation box}
    \item{NW.coord}{North-west coordinates of SD Box}
    \item{NE.coord}{North-east coordinates of SD Box}
    \item{SW.coord}{South-west coordinates of SD Box}
    \item{SE.coord}{South-east coordinates of SD Box}
}
\author{Tarmo K. Remmel, Randy Bui, Ron N. Buliung}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. Pass the entire object to plot_box() and the function automatically extracts this information. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each SD BOX has a unique identifier.}
\seealso{
  \code{\link{plot_box}}, \code{\link{calc_sde}}, \code{\link{calc_sdd}},
  \code{\link[Hmisc]{wtd.var}} 
}
\examples{
# BOX EXAMPLE
data(activities)
a <- calc_box(id=1, centre.xy=NULL, points=activities)
str(a)
print(a)

# IF THE RESULT OF THIS FUNCTION IS STORED TO AN OBJECT, THE plot_box()
# FUNCTION WILL TAKE THAT OBJECT AS INPUT FOR PLOTTING VIA THE datin ARGUMENT

# BOX TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "BOX_Shape", arcgis=T)


}
\keyword{arith}
