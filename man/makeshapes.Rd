\name{makeshapes}
\alias{makeshapes}
\title{Builder of ESRI Shapefiles}
\description{This function is a basic ESRI Shapefile builder.  The SDD, SDE, and MCP functions included in this library produce ASCII output files that represent the input required for this function. However, any similarly formatted file will also work.  This function provides capabilities for converting the centrographic and geometric summary measures of geographical extent and dispersion into GIS databases for further cartographic rendering and analysis.}
\usage{makeshapes(asciiname = "SDD_Output.txt", headerskip = 0, outname = "Test", verbose = TRUE)}
\arguments{
  \item{asciiname}{The name of the ASCII file containing coordinate info. input to the shape building procedure}
  \item{headerskip}{An integer to indicate how many lines of the ASCII file to skip at the top if a header has been added}
  \item{outname}{The name of the output Shapefile.  Do not use spaces or illegal filename characters.}
  \item{verbose}{Boolean: Set to TRUE if extended feedback to the standard output is required}
}
\details{The level of detail recorded in the Shapefile will be determined by the weed tolerance of points defining the shapes in the input ASCII file.}
\value{The result is an ESRI format Shapefile containing 3 files (.shp, .dbf, .shx).  The base filename will be that specified by the parameter \code{outname}.}
\author{Tarmo K. Remmel with significant help from Rick Reeves - NCEAS}
\note{ 
  Currently, the unique identifier is the only attribute that separates polygon objects within
  the Shapefile.  Once the Shapefile is built, a collection of other attributes can be joined to
  the database via the unique identifier.
}
\seealso{
  \code{\link{calc_sdd}}, \code{\link{calc_sde}}, 
  \code{\link{calc_mcp}} 
}
\examples{
  calc_sdd(weights=NULL)
  makeshapes(asciiname="SDD_Output.txt", headerskip=0, outname="Test", verbose=TRUE)
}
\keyword{manip}
