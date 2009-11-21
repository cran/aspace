\name{calc_sde}
\alias{calc_sde}
\title{Calculating the Standard Deviation Ellipse}
\description{This function computes the Standard Deviation Ellipse (SDE) from a set of points. The SDE is a centrographic measure used to characterize the dispersion of point observations along two orthogonal axes. The SDE also captures directional bias in a spatial point pattern, the ellipse will be oriented in the direction of maximum dispersion.}
\usage{calc_sde(id=1, filename="SDE_Output.txt", centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, CMD.npts=10000, points=activities, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify the shape}
  \item{filename}{A string indicating the ASCII textfile where shape coordinates will be written}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDE centre (Planar Coordinates Only!)}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{CMD.npts}{The approximate number of points to generate for the centre of minimum distance (CMD) calculation}
  \item{points}{A 2-column matrix or data frame containing point coordinates}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{This function is most powerful when used repetitively within a loop to compute the SDE for subsets of points stored in a large data table.}
\value{
  The returned result is a list:
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
  \item{central.x}{X-coordinate of the identified central feature}
  \item{central.y}{Y-coordinate of the identified central feature}
  \item{median.x}{X-coordinate of median centre, median value of the X-coordinate vector}
  \item{median.y}{Y-coordinate of median centre, median value of the Y-coordinate vector}
  \item{CMD.x}{X-coordinate of estimated centre of minimum distance}
  \item{CMD.y}{Y-coordinate of estimated centre of minimum distance}
}
\references{See chapter 4 of the documentation manual for CrimeStat at http://www.icpsr.umich.edu/CRIMESTAT/ and Ebdon, D. 1987. Statistics in geography. 2nd edition. New York, NY Basil Blackwell Ltd. 232 p.}
\author{Randy Bui, Ron N. Buliung, Tarmo K. Remmel}
\note{Results are stored in the r.SDE object (required for graphical visualization using plot_sde). This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id parameter to ensure that each SDE has a unique identifier. The output ASCII coordinate file can be further processed using the makeshapes function to generate an ESRI Shapefile for SDE polygons.}
\seealso{
  \code{\link{plot_sde}}, \code{\link{calc_sdd}}, 
  \code{\link{calc_mcp}}, \code{\link{calc_box}}
  \code{\link{makeshapes}}, \code{\link[adehabitat]{mcp}},
  \code{\link[splancs]{gridpts}}, \code{\link{ellipse3}}
}
\examples{calc_sde(id=1, filename="SDE_Output.txt", centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, CMD.npts=10000, points=activities, verbose=TRUE)}
\keyword{arith}