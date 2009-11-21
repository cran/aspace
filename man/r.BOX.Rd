\name{r.BOX}
\alias{r.BOX}
\docType{data}
\title{Demo Data: Standard Deviation Box Output Object}
\description{Results from the Standard Deviation Box Calculator (calc_box) are stored in a list object. This object is required for the plot function (plot_box).}
\usage{data(r.BOX)}
\format{
  The list object contains the following results:
  \describe{
    \item{id}{Identifier for the SD box}
    \item{points}{a simple two-column data frame (or matrix) containing x,y coordinates for a series of point locations.}
    \item{calcentre}{Boolean: Indicates whether the mean centre was computed}
	\item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
    \item{weights}{Weights applied to point observations}
    \item{CENTRE.x}{Actual, used x-coordinate of centre}
    \item{CENTRE.y}{Actual, used y-coordinate of centre}
	\item{central.x}{X-coordinate of central feature}
	\item{central.y}{Y-coordinate of central feature}
	\item{median.x}{X-coordinate of median centre}
	\item{median.y}{Y-coordinate of median centre}
	\item{CMD.x}{X-coordinate of centre of minimum distance}
	\item{CMD.y}{Y-coordinate of centre of minimum distance}
    \item{SDD}{Standard deviation distance value}
    \item{SDx}{Orthogonal standard deviation in x-direction}
    \item{SDy}{Orthogonal standard deviation in y-direction}
    \item{Box.area}{Area of orthogonal standard deviation box}
    \item{NW.coord}{Coordinates of the north-west extent of the SD Box}
	\item{NE.coord}{Coordinates of the north-east extent of the SD Box}
	\item{SW.coord}{Coordinates of the south-west extent of the SD Box}
	\item{SE.coord}{Coordinates of the south-east extent of the SD Box}
  }
}
\details{The coordinates of the points must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(r.BOX)
  str(r.BOX)
}
\keyword{datasets}
