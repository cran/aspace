\name{r.SDE}
\alias{r.SDE}
\docType{data}
\title{Demo Data: Standard Deviation Ellipse Output Object}
\description{Results from the Standard Deviation Ellipse Calculator (calc_sde) are stored in a list object. This object is required for the plot function (plot_sde).}
\usage{data(r.SDE)}
\format{
  The list object contains the following results:
  \describe{
	\item{id}{Identifier for the SDE estimate - it should be unique}
	\item{points}{a simple two-column data frame (or matrix) containing x,y coordinates for a series of point locations.}
	\item{calccentre}{Boolean: TRUE if mean centre is computed}
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
	\item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
    \item{weights}{Weights applied to point observations}
	\item{central.x}{X-coordinate of central feature}
	\item{central.y}{Y-coordinate of central feature}
	\item{median.x}{X-coordinate of median centre}
	\item{median.y}{Y-coordinate of median centre}
	\item{CMD.x}{X-coordinate of centre of minimum distance}
	\item{CMD.y}{Y-coordinate of centre of minimum distance}
  }
}
\details{The coordinates of the points must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(r.SDE)
  str(r.SDE)
}
\keyword{datasets}
