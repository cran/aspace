\name{calc.sdd}
\alias{calc.sdd}
\title{Calculate and plot the Standard Distance Deviation (Standard Distance), and a Standard Deviation Box}
\description{The dispersion of a set of points on a Cartesian plane can be described using the Standard Distance Deviation (SDD) or Standard Distance. For the purpose of geographic visualization, the SDD is typically portrayed as a circle with radius SDD centered on the mean center of a set of point observations. The orthogonal dispersion of a set of points can also be described using the standard deviation of the x- and y-coordinates of a set of point observations. The standard deviation of x- and y-coordinates can be geographically visualized using a box, with the edges set, respectively, to the standard deviation of the x- and y-coordinates.}
\usage{calc.sdd(id = 1, filename = "SDD_Output.txt", centre.xy = centre, calccentre = TRUE, useWMC = FALSE, weightpoints = FALSE, weights = wts, destmat = activities, verbose = FALSE, plot = TRUE, plothv = TRUE, plotdest = TRUE, plotcenter = TRUE, box = TRUE)}
\arguments{
  \item{id}{A unique integer to identify the shape}
  \item{filename}{A string indicating the ASCII textfile where shape coordinates will be written}
  \item{centre.xy}{A vector of length 2, containing the x- and y-coordinates of the SDD centroid}
  \item{calccentre}{Boolean: Set to TRUE if the mean center is to be calculated}
  \item{useWMC}{Boolean: Set to TRUE if the mean center is to be computed with weighted coordinates}
  \item{weightpoints}{Boolean: Set to TRUE if the point observations are to be weighted}
  \item{weights}{Weights applied to point observations}
  \item{destmat}{A 2-column matrix or data frame containing point coordinates}
  \item{verbose}{Boolean: Set to TRUE if extensive feedback is desired on the standard output}
  \item{plot}{Boolean: Set to TRUE if the SDD is to be plotted}
  \item{plothv}{Boolean: Set to TRUE if the orthogonal N-S, E-W axes are to be plotted through the center}
  \item{plotdest}{Boolean: Set to TRUE if the point observations are to be plotted}
  \item{plotcenter}{Boolean: Set to TRUE if the mean center is to be plotted}
  \item{box}{Boolean: Set to TRUE if the standard deviation of the x- and y-coordinates are to be plotted as a box}
}
\details{This function is most powerful when used repetitively within a loop to compute the SDD for subsets of points stored in a large table.}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the SDD shape - it should be unique}
  \item{calccentre}{True if mean centre is computed}
  \item{Orig.x}{Original x-coordinate of center before mean center calculation}
  \item{Orig.y}{Original y-coordinate of center before mean center calculation}
  \item{CENTRE.x}{Actual, used x-coordinate of centre}
  \item{CENTRE.y}{Actual, used y-coordinate of centre}
  \item{SD.x}{Standard deviation of the x-coordinates}
  \item{SD.y}{Standard deviation of the y-coordinates}
  \item{SDD.radius}{SDD value, radius of the SDD}
  \item{Box.area}{Area of the box formed by the standard deviation of the x- and y-coordinates}
  \item{SDD.area}{Area of the SDD circle}
  \item{useWMC}{Boolean: TRUE if the weighted mean center is used}
  \item{WeightPoints}{Boolean: TRUE if point observations are weighted}
}
\author{Tarmo K. Remmel, Ron Buliung}
\note{This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table.  When used repetitively, be sure to increment the id parameter to ensure that each SDD has a unique identifier.  The output ASCII coordinate file can be further processed using the makeshapes function to generate an ESRI Shapefile for SDD polygons.}
\seealso{
  \code{\link{ellipse3}}, \code{\link{calc.mcp}},
  \code{\link{calc.sde}}, \code{\link{makeshapes}}
}
\examples{calc.sdd(id = 1, filename = "SDD_Output.txt", centre.xy = centre, calccentre = TRUE, useWMC = FALSE, weightpoints = FALSE, destmat = activities, verbose = FALSE, plot = TRUE, plothv = TRUE, plotdest = TRUE, plotcenter = TRUE, box = TRUE)}
\keyword{arith}
