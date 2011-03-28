\name{calc_box}
\alias{calc_box}
\title{Calculate the Standard Deviation Box}
\description{The orthogonal dispersion of a set of points can be described using the standard deviation of the x- and y-coordinates of a set of point observations. The orthogonal dispersion can then be visualized with a Standard Deviation Box. This function computes the properties of the Standard Deviation Box (SD Box) from a set of point observations.}
\usage{calc_box(id=1, filename="BOX_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=FALSE, weights=NULL, points=activities, verbose=FALSE)}
\arguments{
  \item{id}{A unique integer to identify a SD Box}
  \item{filename}{A string indicating the ASCII textfile where the box coordinates will be written}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the geographic centre of the SD Box}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations input to the calc_box function}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
}
\details{Use the boxloc (coordinates) and boxatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The returned result is a list:
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
\author{Randy Bui, Ron N. Buliung, Tarmo K. Remmel}
\note{Results are stored in the r.BOX object (required for plot_box). This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table. When used repetitively, be sure to increment the id parameter to ensure that each SD BOX has a unique identifier. The output ASCII coordinate file can be further processed using the shapefiles package to generate an ESRI Shapefile for SD BOX polygons.}
\seealso{
  \code{\link{plot_box}}, \code{\link{calc_sde}}, \code{\link{calc_sdd}},
  \code{\link[Hmisc]{wtd.var}} 
}
\examples{
## BOX example
calc_box(id=1, filename="BOX_Output.txt", centre.xy=NULL, calccentre=TRUE, 
weighted=FALSE, weights=NULL, points=activities, verbose=FALSE)

## SD Box to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(boxloc,boxatt,"id",5)
## write.shapefile(shp, "BOX_Shape", arcgis=T)
}
\keyword{arith}
