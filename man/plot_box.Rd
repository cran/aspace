\name{plot_box}
\alias{plot_box}
\title{Plot the Standard Distance Box}
\description{This function plots the standard deviation of x- and y-coordinates as a box, with the edges set, respectively, to the standard deviation of the x- and y-coordinates.}
\usage{plot_box(datin=NULL, plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE,
weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, 
points.col='black', points.pch=1, plotcentre=TRUE, centre.col='black', 
centre.pch=19, titletxt="Title", xaxis="Easting (m)", 
yaxis="Northing (m)", box.col='black', box.lwd=2, jpeg=FALSE, ...)}
\arguments{
  \item{datin}{Input data object; the result from calc_box()}
  \item{plotnew}{Boolean: Set to TRUE to create a new plot. Set to FALSE to overlay current plot.}
  \item{plothv}{Boolean: Set to TRUE if the orthogonal N-S, E-W axes are to be plotted through the centre}
  \item{plotweightedpts}{Boolean: Set to TRUE if the weighted point observations are to be plotted}
  \item{weightedpts.col}{Specify a colour for the weighted point observations}
  \item{weightedpts.pch}{Specify a plotting symbol for the weighted point observations}
  \item{plotpoints}{Boolean: Set to TRUE if the point observations are to be plotted}
  \item{points.col}{Specify a colour for the point observations}
  \item{points.pch}{Specify a plotting symbol for the point observations}
  \item{plotcentre}{Boolean: Set to TRUE if the mean/weighted/user-defined centre is to be plotted}
  \item{centre.col}{Specify a colour for the centre}
  \item{centre.pch}{Specify a plotting symbol for the centre}
  \item{titletxt}{A string to indicate the title for the plot}
  \item{xaxis}{A string to label the x-axis of the plot}
  \item{yaxis}{A string to label the y-axis of the plot}
  \item{box.col}{Specify a line colour for the SD Box}
  \item{box.lwd}{Specify a line width for the SD Box}
  \item{jpeg}{Boolean: Set to TRUE if the plot should be saved in JPEG format}
  \item{...}{Arguments to be passed to graphical parameters}
}
\value{This function returns a plot in the graphics device.}
\details{The element FORPLOTTING contained within the calc_box() output object is required to plot an SD Box. Provide the whole plot_box() output object as the argment for datin.}
\author{Tarmo K. Remmel, Randy Bui, Ron N. Buliung}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{plot_sde}}
}
\examples{
# NEED TO RUN THE BOX GENERATOR FIRST AND PASS THAT TO THE NEXT LINE
a <- calc_box(id=1, points=activities)
plot_box(datin=a, plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE,
plotpoints=TRUE, plotcentre=TRUE, titletxt="Title", 
xaxis="Easting (m)", yaxis="Northing (m)")

# plot_box() BY DEFAULT, TAKES AS INPUT THE RESULT PRODUCED BY calc_box()
}
\keyword{arith}
