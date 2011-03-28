\name{plot_centres}
\alias{plot_centres}
\title{Plot centres}
\description{This function plots various centre of a set of point observations.}
\usage{plot_centres(plotnew=FALSE, plotSDE=FALSE, xaxis="Easting (m)", yaxis="Northing (m)",
robject=NULL, plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, 
plotpoints=TRUE, points.col='black', points.pch=1, plotcentre=FALSE, centre.col='black', 
centre.pch=19, plotcentral=FALSE, central.col='green', central.pch=19, 
plotCF2PTS=FALSE, CF2PTS.col='orange', CF2PTS.pch=19, plotmedian=FALSE, 
median.col='blue', median.pch=17, plotCMD=FALSE, CMD.col='red', CMD.pch=17, ...)}
\arguments{
  \item{plotnew}{Boolean: Set to TRUE to create a new plot. Set to FALSE to overlay current plot.}
  \item{plotSDE}{Boolean: Set to TRUE if the centres for the SDE are to be plotted}
  \item{xaxis}{A string to label the x-axis of the plot}
  \item{yaxis}{A string to label the y-axis of the plot}
  \item{robject}{Specify the results object from the computation function. Can be either r.SDD, r.SDE, or r.BOX.}
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
  \item{plotCF2PTS}{Boolean: Set to TRUE if the central feature between 2 point patterns is to be highlighted}
  \item{CF2PTS.col}{Specify a colour for the central feature}
  \item{CF2PTS.pch}{Specify a plotting symbol for the central feature}
  \item{plotmedian}{Boolean: Set to TRUE if the median centre is to be plotted}
  \item{median.col}{Specify a colour for the median centre}
  \item{median.pch}{Specify a plotting symbol for the median centre}
  \item{plotCMD}{Boolean: Set to TRUE if the centre of minimum distance is to be plotted}
  \item{CMD.col}{Specify a colour for the centre of minimum distance}
  \item{CMD.pch}{Specify a plotting symbol for the centre of minimum distance}
  \item{...}{Arguments to be passed to graphical parameters}
}
\details{The results object, for example, r.SDD object (generated in calc_sdd function) is required to plot the centres for the SDD.}
\author{Randy Bui, Ron N. Buliung, Tarmo K. Remmel}
\seealso{
  \code{\link{plot_sde}}, \code{\link{plot_box}}  
}
\examples{plot_centres(plotnew=FALSE, plotSDE=FALSE, robject=NULL, plotweightedpts=FALSE,
xaxis="Easting (m)", yaxis="Northing (m)",
weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, 
points.col='black', points.pch=1, plotcentre=FALSE, centre.col='black', 
centre.pch=19, plotcentral=FALSE, central.col='green', central.pch=19, 
plotCF2PTS=FALSE, CF2PTS.col='orange', CF2PTS.pch=19,
plotmedian=FALSE, median.col='blue', median.pch=17, plotCMD=FALSE, 
CMD.col='red', CMD.pch=17)}
\keyword{arith}
