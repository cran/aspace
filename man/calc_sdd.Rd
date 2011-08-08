\name{calc_sdd}
\alias{calc_sdd}
\title{Calculate the Standard Distance Deviation (Standard Distance)}
\description{This function computes the Standard Distance Deviation (SDD) or Standard Distance from a set of points.}
\usage{calc_sdd(id=1, filename="SDD_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=FALSE, weights=NULL, points=activities, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify a SDD estimate}
  \item{filename}{A string indicating the ASCII textfile where shape coordinates will be written}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDD centre}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations input to the calc_sdd function}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{Use the sddloc (coordinates) and sddatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the SDD shape - it should be unique}
  \item{calccentre}{Boolean: TRUE if mean centre is computed}
  \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
  \item{CENTRE.x}{X-coordinate of the centre}
  \item{CENTRE.y}{Y-coordinate of the centre}
  \item{SDD.radius}{SDD value, radius of the SDD}
  \item{SDD.area}{Area of the SDD circle}
}
\author{Randy Bui, Ron Buliung, Tarmo K. Remmel}
\note{Results are stored in the r.SDD object (required for plot_sdd). This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id parameter to ensure that each SDD has a unique identifier. The output ASCII coordinate file can be further processed using the shapefiles package to generate an ESRI Shapefile for SDD polygons.}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{calc_sde}}, 
  \code{\link{calc_box}}
}
\examples{
## SDD example
calc_sdd(id=1, filename="SDD_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=FALSE, weights=NULL, points=activities, verbose=FALSE)

## plot_sdd by default takes as input the result produced from the calc_sdd, read from the current workspace. 

## SDD to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(sddloc,sddatt,"id",5)
## write.shapefile(shp, "SDD_Shape", arcgis=T)
}

\keyword{arith}