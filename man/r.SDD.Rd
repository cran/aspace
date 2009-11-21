\name{r.SDD}
\alias{r.SDD}
\docType{data}
\title{Demo Data: Standard Deviation Distance Output Object}
\description{Results from the Standard Deviation Distance Calculator (calc_sdd) are stored in a list object. This object is required for the plot function (plot_sdd).}
\usage{data(r.SDD)}
\format{
  The list object contains the following results:
  \describe{
	\item{id}{Identifier for the SDD estimation - it should be unique}
    \item{points}{a simple two-column data frame (or matrix) containing x,y coordinates for a series of point locations.}
	\item{SDD}{SDD value, radius of the SDD}
	\item{calccentre}{Boolean: TRUE if mean centre is computed}
	\item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
    \item{weights}{Weights applied to point observations}
	\item{CENTRE.x}{X-coordinate of the centre}
	\item{CENTRE.y}{Y-coordinate of the centre}
	\item{central.x}{X-coordinate of central feature}
	\item{central.y}{Y-coordinate of central feature}
	\item{median.x}{X-coordinate of median centre}
	\item{median.y}{Y-coordinate of median centre}
	\item{CMD.x}{X-coordinate of centre of minimum distance}
	\item{CMD.y}{Y-coordinate of centre of minimum distance}
	\item{SDD.area}{Area of the SDD circle}	
  }
}
\details{The coordinates of the points must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(r.SDD)
  str(r.SDD)
}
\keyword{datasets}

