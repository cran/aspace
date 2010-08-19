\name{calc_sdd}
\alias{calc_sdd}
\title{Calculate the Standard Distance Deviation (Standard Distance)}
\description{This function computes the Standard Distance Deviation (SDD) or Standard Distance from a set of points.}
\usage{calc_sdd(id=1, filename="SDD_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=FALSE, weights=NULL, CMD.npts=10000, points=activities, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify a SDD estimate}
  \item{filename}{A string indicating the ASCII textfile where shape coordinates will be written}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDD centre}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{CMD.npts}{The approximate number of grid points within the convex hull for the centre of minimum distance (CMD) calculation}
  \item{points}{A 2-column matrix or data frame containing the set of point observations input to the calc_sdd function}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{This function is most powerful when used repetitively within a loop to compute the SDD for subsets of points stored in a large table.}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the SDD shape - it should be unique}
  \item{calccentre}{Boolean: TRUE if mean centre is computed}
  \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
  \item{CENTRE.x}{X-coordinate of the centre}
  \item{CENTRE.y}{Y-coordinate of the centre}
  \item{central.x}{X-coordinate of the identified central feature}
  \item{central.y}{Y-coordinate of the identified central feature}
  \item{median.x}{X-coordinate of median centre, median value of the X-coordinate vector}
  \item{median.y}{Y-coordinate of median centre, median value of the Y-coordinate vector}
  \item{CMD.x}{X-coordinate of estimated centre of minimum distance}
  \item{CMD.y}{Y-coordinate of estimated centre of minimum distance}
  \item{SDD.radius}{SDD value, radius of the SDD}
  \item{SDD.area}{Area of the SDD circle}
}
\author{Randy Bui, Ron Buliung, Tarmo K. Remmel}
\note{Results are stored in the r.SDD object (required for graphical visualization using plot_sdd). As the value of 'CMD.npts' increases, the more precise the centre of minimum distance is estimated. This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id parameter to ensure that each SDD has a unique identifier. The output ASCII coordinate file can be further processed using the makeshapes function to generate an ESRI Shapefile for SDD polygons.}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{calc_mcp}},
  \code{\link{calc_sde}}, \code{\link{calc_box}},
  \code{\link{ellipse3}}, \code{\link{makeshapes}},
  \code{\link[adehabitat]{mcp}}, \code{\link[splancs]{gridpts}}  
}
\examples{calc_sdd(id=1, filename="SDD_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=TRUE, weights=wts, CMD.npts=10000, points=activities, verbose=TRUE)}
\keyword{arith}