\name{calc_sde}
\alias{calc_sde}
\title{Calculate and plot a Standard Deviation Ellipse}
\description{This function computes the Standard Deviation Ellipse (SDE) for a set of points. The SDE is a centrographic measure used to characterize the dispersion of point observations along two orthogonal axes. The SDE also captures directional bias in aspatial point pattern and will be oriented in the direction of maximum dispersion. The function provides options for weighting observations, and centring the ellipse on a user defined point, mean centre, or weighted mean centre of the input point locations. Output includes plotting of the SDE and an ASCII (text) file containing elliptical coordinates.}
\usage{calc_sde(id = 1, filename = "SDE_Output.txt", calccentre = FALSE, useWMC = FALSE, centre.xy = centre, destmat = activities, titletxt = "Title", verbose = FALSE, plot = TRUE, calcSDxy = TRUE, plotSDEaxes = TRUE, plotdest = TRUE, plotcentroid = TRUE, plotSDxy = TRUE, weightpoints = FALSE, weights = wts, jpeg = FALSE)}
\arguments{
  \item{id}{An identifier for a given SDE.  When running calc.sde in a loop for multiple sets of points, increment id such that it is unique!}
  \item{filename}{The name of an ASCII (text) file where SDE coordinates will be written}
  \item{calccentre}{Boolean: Set to TRUE if the mean center of the points is to be used as the SDE centroid}
  \item{useWMC}{Boolean: Set to TRUE if the weighted mean center is to be used as the SDE centroid}
  \item{centre.xy}{A numeric vector of length 2, containing specified x- and y-coordinates to use as the centroid}
  \item{destmat}{A numeric matrix or data frame with two columns.  The first column represents x-coordinates, the second, y-coordinates.  Each row corresponds to a single point location.}
  \item{titletxt}{A string to use as the title on the plot}
  \item{verbose}{Boolean: Set to TRUE if extensive standard output feedback is desired}
  \item{plot}{Boolean: Set to TRUE if the SDE is to be plotted}
  \item{calcSDxy}{Boolean: Set to TRUE if the standard deviations in the orthogonal (x and y) directions are to be computed}
  \item{plotSDEaxes}{Boolean: Set to TRUE if the orthogonal axes through the centroid are to be plotted}
  \item{plotdest}{Boolean: Set to TRUE if input point observations are to be plotted along with the SDE}
  \item{plotcentroid}{Boolean: Set to TRUE if the centroid is to be plotted along with the SDE}
  \item{plotSDxy}{Boolean: Set to TRUE if the orthogonal standard deviation box should be plotted along with the SDE}
  \item{weightpoints}{Boolean: Set to TRUE if the point observations are to be weighted}
  \item{weights}{A matrix or data frame of weights for the points}
  \item{jpeg}{Boolean: Set to TRUE if the plot should be saved in JPEG format}
}
\details{This function is most powerful when used repetitively within a loop to compute the SDE for subsets of points stored in a large data table.}
\value{
  The returned result is a list:
  \item{CALCCENTRE}{Boolean: Indicates whether the mean centre was computed}
  \item{WeightPoints}{Boolean: Indicates whether the points were weighted}
  \item{UseWMC}{Boolean: Indicates whether the weighted mean centre is to be used}
  \item{Orig.x}{Original x-coordinate of centre}
  \item{Orig.y}{Original y-coordinate of centre}
  \item{CENTRE.x}{x-coordinate after computation of mean centre}
  \item{CENTRE.y}{y-coordinate after computation of mean centre}
  \item{Sigma.x}{Half-length of axis along x-axis}
  \item{Sigma.y}{Half-length of axis along y-axis}
  \item{Major}{String indicating which axis is the major elliptical axis}
  \item{Minor}{String indicating which axis is the minor elliptical axis}
  \item{Theta}{Rotation angle in degrees}
  \item{Eccentricity}{A measure of eccentricity}
  \item{Area.sde}{Area of the SDE}
  \item{TanTheta}{Trigonometric result}
  \item{SinTheta}{Trigonometric result}
  \item{CosTheta}{Trigonometric result}
  \item{SinThetaCosTheta}{Trigonometric result}
  \item{Sin2Theta}{Trigonometric result}
  \item{Cos2Theta}{Trigonometric result}
  \item{ThetaCorr}{Corrected theta angle for rotation of major axis from north}
  \item{WMC.x}{Weighted mean center x-coordinate}
  \item{WMC.y}{Weighted mean center y-coordinate}
}
\references{See chapter 4 of the documentation manual for CrimeStat at http://www.icpsr.umich.edu/CRIMESTAT/ and Ebdon, D. 1987. Statistics in geography. 2nd edition. New York, NY Basil Blackwell Ltd. 232 p.}
\author{Tarmo K. Remmel, Ron Buliung}
\note{This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table.  When used repetitively, be sure to increment the id parameter to ensure that each SDE has a unique identifier.  The output ASCII coordinate file can be further processed using the makeshapes function to generate an ESRI Shapefile for SDE polygons.}
\seealso{
  \code{\link{calc_sdd}}, \code{\link{calc_mcp}}, 
  \code{\link{makeshapes}}
}
\examples{calc_sde(id = 1, filename = "SDE_Output.txt", calccentre = FALSE, useWMC = FALSE, centre.xy = centre, destmat = activities, titletxt = "Title", verbose = FALSE, plot = TRUE, calcSDxy = TRUE, plotSDEaxes = TRUE, plotdest = TRUE, plotcentroid = TRUE, plotSDxy = TRUE, weightpoints = FALSE, weights = wts, jpeg = FALSE) }
\keyword{arith}
