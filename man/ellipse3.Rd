\name{ellipse3}
\alias{ellipse3}
\title{Ellipse drawing tool}
\description{A convenient tool for plotting circles or ellipses with the functionality of rotation about an angle theta (in radians).  The function can also be used to capture coordinates of the perimeter of the shape for further usage outside the function.}
\usage{ellipse3(cx, cy, rx, ry, theta = 0, yaxis = TRUE, 
pointsonly = FALSE, fill = FALSE, ...)}
\arguments{
  \item{cx}{x-coordinate of ellipse center}
  \item{cy}{y-coordinate of ellipse center}
  \item{rx}{radius along x-axis}
  \item{ry}{radius along y-axis}
  \item{theta}{rotation angle in radians from north}
  \item{yaxis}{Depreciated. This parameter adjusts the size correct in the y- or x-axis, as plotting is generally not square.  Therefore, use par(pty="s") to eliminate the need for this parameter.}
  \item{pointsonly}{Boolean: If TRUE, ellipse will not be plotted, but rather the coordinates of the perimeter are returned in a list object for further use.  The calc.sde function utilizes these coordinates to build a textfile of coordinates which the makeshapes function uses to build ESRI Shapefiles.}
  \item{fill}{Boolean: If TRUE, the plotted ellipse will be shaded in}
  \item{\dots}{Any additional parameters suitable for plotting}
}
\details{This function plots an ellipse with center (cx, cy).  The rotation angle in radians (form north) is given as theta.  Note that a circle can be obtained by rx=ry, in which case theta is not very useful.  Additional parameters (e.g., colour and fill density) can be provided as indicated by the '...' in the function call.}
\value{
  This function returns a plot of an ellipse when pointsonly = FALSE.
  When pointsonly = TRUE, the result is a list of x,y coordinates
  \item{x}{A numeric vector of x-coordinates}
  \item{y}{A numeric vector of y-coordinates}
}
\author{Tarmo K. Remmel}
\note{This function is an an adjustment by Brad Biggerstaff (CDC) and Tarmo K. Remmel of the function circle() written by John Wallace (University of Washington) and obtained from the S-News listserv.}
\seealso{ 
  \code{\link{calc_sde}}, \code{\link{as_radians}}
}
\examples{
  plot.new()
  plot(10,10, type="n")
  ellipse3(cx = 10, cy = 8, rx = 2, ry = 1, theta = as_radians(45), 
  yaxis = TRUE, pointsonly = FALSE, fill = FALSE, col=6)
}
\keyword{graphs}
