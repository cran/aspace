\name{plot_sde}
\alias{plot_sde}
\title{Plot the Standard Deviation Ellipse}
\description{This function plots the SDE as an ellipse centred on the mean/weighted/user-defined centre of a set of point observations. The plot characterizes the dispersion of point observations along two orthogonal axes.}
\usage{plot_sde(datin=NULL, plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE,
weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, 
points.col='black', points.pch=1, plotcentre=TRUE, centre.col='black', 
centre.pch=19, titletxt="Title", xaxis="Easting (m)", 
yaxis="Northing (m)", sde.col='black', sde.lwd=2, jpeg=FALSE, ...)}
\arguments{
  \item{datin}{Input data object; the result from calc_sde()}
  \item{plotnew}{Boolean: Set to TRUE to create a new plot. Set to FALSE to overlay current plot.}
  \item{plotSDEaxes}{Boolean: Set to TRUE if the orthogonal axes through the centroid are to be plotted}
  \item{plotweightedpts}{Boolean: Set to TRUE if the weighted point observations are to be plotted}
  \item{weightedpts.col}{Specify a colour for the weighted point observations}
  \item{weightedpts.pch}{Specify a plotting symbol for the weighted point observations}
  \item{plotpoints}{Boolean: Set to TRUE if the point observations are to be plotted}
  \item{points.col}{Specify a colour for the point observations}
  \item{points.pch}{Specify a plotting symbol for the point observations}
  \item{plotcentre}{Boolean: Set to TRUE if the mean/weighted/user-defined centre is to be plotted}
  \item{centre.col}{Specify a colour for the centre}
  \item{centre.pch}{Specify a plotting symbol for the centre}
  \item{titletxt}{A string to indicate the title on the plot}
  \item{xaxis}{A string to label the x-axis of the plot}
  \item{yaxis}{A string to label the y-axis of the plot}
  \item{sde.col}{Specify a line colour for the SDE circle}
  \item{sde.lwd}{Specify a line width for the SDE circle}
  \item{jpeg}{Boolean: Set to TRUE if the plot should be saved in JPEG format}
  \item{...}{Arguments to be passed to graphical parameters}
}
\value{This function returns a plot in the graphics device.}
\details{The element FORPLOTTING containe within the calc_box() output object is required to plot an SD Box. Provide the whole plot_box() output object as the argment for datin.}
\author{Tarmo K. Remmel, Randy Bui, Ron N. Buliung}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{plot_box}}  
}
\examples{
a <- calc_sde(points=activities)
plot_sde(datin=a, plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE,
plotpoints=TRUE, plotcentre=TRUE, titletxt="Title", 
xaxis="Easting (m)", yaxis="Northing (m)")

# plot_sde() BY DEFAULT, TAKES AS INPUT THE RESULT PRODUCED BY calc_sde()
}
\keyword{arith}
