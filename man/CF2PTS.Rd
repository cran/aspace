\name{CF2PTS}
\alias{CF2PTS}
\title{Central feature between 2 point patterns (CF2PTS) Calculator}
\description{Central feature of point2 within point1. Identifies the central feature as the point location in the first pattern that has the smallest cummulative distance to features in a second point pattern.}
\usage{CF2PTS(id=1, filename="CF2PTS_Output.txt", points1=activities, points2=activities2)}
\arguments{
  \item{id}{A unique integer to identify the CF2PTS}
  \item{filename}{A string indicating the ASCII textfile where the central feature coordinates will be written}
  \item{points1}{A 2-column matrix or data frame containing the set of point observations}
  \item{points2}{A 2-column matrix or data frame containing the set of point observations}
}
\details{Use the cf2ptsloc (coordinates) and cf2ptsatt (attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the central feature - it should be unique}
  \item{CF2PTS.x}{X-coordinate of the central feature}
  \item{CF2PTS.y}{Y-coordinate of the central feature}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.CF2PTS object and can be passed through plotting functions. This function can also be used repetitively within a loop to compute multiple CF2PTS centres from different datasets.}
\seealso{
  \code{\link{CF}}, \code{\link{CMD}},
  \code{\link{median_centre}}
}
\examples{
## CF2PTS example
CF2PTS(id=1, filename="CF2PTS_Output.txt", points1=activities, points2=activities2)

## CF2PTS to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(cf2ptsloc,cf2ptsatt,"id",5)
## write.shapefile(shp, "CF2PTS_Shape", arcgis=T)
}
\keyword{arith}