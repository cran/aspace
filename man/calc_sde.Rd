\name{calc_sde}
\alias{calc_sde}
\title{Calculate the Standard Deviation Ellipse}
\description{This function computes the Standard Deviation Ellipse (SDE) from a set of points. The SDE is a centrographic measure used to characterize the dispersion of point observations along two orthogonal axes. The SDE also captures directional bias in a spatial point pattern, the ellipse will be oriented in the direction of maximum dispersion.}
\usage{calc_sde(id=1, centre.xy=NULL, calccentre=TRUE,
weighted=FALSE, weights=NULL, points=NULL, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the shape}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDE centre (Planar Coordinates Only!)}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing point coordinates}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{Use the LOCATIONS element in the output list object along with the ATTRIBUTES elements can be used to produce shapefiles or other vector point files for geographic data.}
\value{
   The returned result is a list:
  \item{TYPE}{The type of calculation results stored in the object: BOX, SDD, SDE, CMD, CF, or CF2PTS, MNC, MDC}
  \item{DATE}{The date and time that the function was run}
  \item{ID}{Identifier for the SDE shape - it should be unique}
  \item{LOCATIONS}{Locations pertinent for the SDE that can be used with ATTRIBUTES if wishing to build a vector point file for geographic data outside of this pacakge.}
  \item{FORPLOTTING}{Coordinates and identifiers used for plotting by plot_sde()}
  \item{ATTRIBUTES}{Attributes for the output SDE that can be used with LOCATIONS coordinates if wishing to build a vector point file for geographic data outside of this package.}
  
    \item{id}{Identifier for the SDE shape - it should be unique}
    \item{calccentre}{Boolean: TRUE if mean centre is computed}
    \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
    \item{CENTRE.x}{X-coordinate of the centre}
    \item{CENTRE.y}{Y-coordinate of the centre}
    \item{Sigma.x}{Half-length of axis along x-axis}
    \item{Sigma.y}{Half-length of axis along y-axis}
    \item{Major}{String indicating which axis is the major elliptical axis}
    \item{Minor}{String indicating which axis is the minor elliptical axis}
    \item{Theta}{Rotation angle in degrees}
    \item{Eccentricity}{A measure of eccentricity (i.e., the flatness of the ellipse)}
    \item{Area.sde}{Area of the SDE}
    \item{TanTheta}{Trigonometric result}
    \item{SinTheta}{Trigonometric result}
    \item{CosTheta}{Trigonometric result}
    \item{SinThetaCosTheta}{Trigonometric result}
    \item{Sin2Theta}{Trigonometric result}
    \item{Cos2Theta}{Trigonometric result}
    \item{ThetaCorr}{Corrected theta angle for rotation of major axis from north}
}
\references{See chapter 4 of the documentation manual for CrimeStat at http://www.icpsr.umich.edu/CRIMESTAT/ and Ebdon, D. 1987. Statistics in geography. 2nd edition. New York, NY Basil Blackwell Ltd. 232 p.}
\author{Tarmo K. Remmel, Randy Bui, Ron N. Buliung}
\note{Results specific for plotting are stored in the FORPLOTTING element within the produced list object. Pass the entire object to plot_box() and the function automatically extracts this information. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id argument to ensure that each SDE has a unique identifier.}
\seealso{
  \code{\link{calc_box}},
  \code{\link{calc_sde}},
  \code{\link{calc_cmd}},
  \code{\link{calc_cf}},
  \code{\link{calc_cf2pts}},
  \code{\link{calc_mnc}},
  \code{\link{calc_mdc}},
  \code{\link[splancs]{gridpts}}
}
\examples{
# SDE EXAMPLE
data(activities)
a <- calc_sde(id=1, centre.xy=NULL, points=activities)
str(a)
print(a)

# IF THE RESULT OF THIS FUNCTION IS STORED TO AN OBJECT, THE plot_sde()
# FUNCTION WILL TAKE THAT OBJECT AS INPUT FOR PLOTTING VIA THE datin ARGUMENT

# SDE TO SHAPEFILE EXAMPLE (REMOVE THE COMMENTS TO RUN)
# shp <- convert.to.shapefile(a$LOCATIONS, a$ATTRIBUTES, "id", 5)
# write.shapefile(shp, "SDE_Shape", arcgis=T)
}
\keyword{arith}
