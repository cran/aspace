\name{plot_box}
\alias{plot_box}
\title{Plot the Standard Distance Box}
\description{This function plots the standard deviation of x- and y-coordinates as a box, with the edges set, respectively, to the standard deviation of the x- and y-coordinates.}
\usage{plot_box(plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, points.col='black', points.pch=1, plotcentre=TRUE, centre.col='black', centre.pch=19, plotcentral=FALSE, central.col='green', central.pch=19, plotmedian=FALSE, median.col='blue', median.pch=17, plotCMD=FALSE, CMD.col='red', CMD.pch=17, titletxt="Title", xaxis="Easting (m)", yaxis="Northing (m)", box.col='black', box.lwd=2, jpeg=FALSE, ...)}
\arguments{
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
  \item{plotcentral}{Boolean: Set to TRUE if the central feature is to be highlighted}
  \item{central.col}{Specify a colour for the central feature}
  \item{central.pch}{Specify a plotting symbol for the central feature}
  \item{plotmedian}{Boolean: Set to TRUE if the median centre is to be plotted}
  \item{median.col}{Specify a colour for the median centre}
  \item{median.pch}{Specify a plotting symbol for the median centre}
  \item{plotCMD}{Boolean: Set to TRUE if the centre of minimum distance is to be plotted}
  \item{CMD.col}{Specify a colour for the centre of minimum distance}
  \item{CMD.pch}{Specify a plotting symbol for the centre of minimum distance}
  \item{titletxt}{A string to indicate the title for the plot}
  \item{xaxis}{A string to label the x-axis of the plot}
  \item{yaxis}{A string to label the y-axis of the plot}
  \item{box.col}{Specify a line colour for the SD Box}
  \item{box.lwd}{Specify a line width for the SD Box}
  \item{jpeg}{Boolean: Set to TRUE if the plot should be saved in JPEG format}
  \item{...}{Arguments to be passed to graphical parameters}
}
\details{The r.BOX object (generated using the calc_box function) is required to plot an SD Box.}
\author{Randy Bui, Ron N. Buliung, Tarmo K. Remmel}
\seealso{
  \code{\link{plot_sdd}}, \code{\link{plot_mcp}},
  \code{\link{plot_sde}}, \code{\link{makeshapes}},
  \code{\link{ellipse3}},  
}
\examples{plot_box(plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE, plotpoints=TRUE, plotcentre=TRUE, plotcentral=TRUE, plotmedian=TRUE, plotCMD=TRUE, titletxt="Title", xaxis="Easting (m)", yaxis="Northing (m)")}
\keyword{arith}