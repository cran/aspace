\name{mean_centre}
\alias{mean_centre}
\title{Mean Centre Calculator}
\description{Compute the mean centre from a series of point locations.}
\usage{mean_centre(id=1, filename="mean_centre_Output.txt", 
weighted=FALSE, weights=NULL, points=activities)}
\arguments{
  \item{id}{A unique integer to identify the mean centre}
  \item{filename}{A string indicating the ASCII textfile where centre coordinates will be written}
  \item{weighted}{Boolean: Set to TRUE if the weighted mean center is to be computed with weighted coordinates}
  \item{weights}{Weights applied to point observations, number of weights should equal the number of observations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{Use the meanloc (coordinates) and meanatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the mean centre - it should be unique}
  \item{weighted}{Boolean: TRUE if the weighted mean centre is to be used instead}
  \item{weights}{Weights applied to point observations}
  \item{CENTRE.x}{X-coordinate of the mean centre}
  \item{CENTRE.y}{Y-coordinate of the mean centre}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.mean object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple mean centres from different datasets.}
\seealso{
  \code{\link{median_centre}}, \code{\link{CMD}},
  \code{\link{CF}}
}
\examples{
## Mean centre example
mean_centre(id=1, filename="mean_centre_Output.txt", 
weighted=FALSE, weights=NULL, points=activities)

## Mean centre to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(meanloc,meanatt,"id",5)
## write.shapefile(shp, "Mean_Shape", arcgis=T)
}
\keyword{arith}
