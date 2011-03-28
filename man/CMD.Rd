\name{CMD}
\alias{CMD}
\title{Centre of Minimum Distance (CMD) Calculator}
\description{Compute the CMD within a set of point locations.}
\usage{CMD(id=1, filename="CMD_Output.txt", dist=100, 
points=activities)}
\arguments{
  \item{id}{A unique integer to identify the CMD}
  \item{filename}{A string indicating the ASCII textfile where centre coordinates will be written}
  \item{dist}{Hold distance value between i and ith iterations}
  \item{points}{A 2-column matrix or data frame containing the set of point observations}
}
\details{Use the cmdloc (coordinates) and cmdatt(attributes) to produce shapefiles using the convert.to.shapefile 
and write.shapefile from the shapefiles library}
\value{
  The result is a list of terms:
  \item{id}{Identifier for the CMD - it should be unique}
  \item{CMD.x}{X-coordinate of the CMD}
  \item{CMD.y}{Y-coordinate of the CMD}
  \item{distance}{Hold distance value between i and ith iterations (metres}
  \item{Number of Cells}{Hold number of cells in each grid created for each iteration}
  }
\author{Randy Bui, Ron Buliung}
\note{Results are stored in the r.CMD object and can be passed through plotting functions. The dist parameter specifies the distance threshold between i and ith iterations. This function can also be used repetitively within a loop to compute multiple CMD centres from different datasets.}
\seealso{
  \code{\link{mean_centre}}, \code{\link{median_centre}},
  \code{\link{CF}}
}
\examples{
## CMD example
CMD(id=1, filename="CMD_Output.txt", dist=100, 
points=activities)

## CMD to shapefile example (exclude the comments below to run script)
## shp <- convert.to.shapefile(cmdloc,cmdatt,"id",5)
## write.shapefile(shp, "CMD_Shape", arcgis=T)
}
\keyword{arith}
