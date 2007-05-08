\name{calc_mcp}
\alias{calc_mcp}
\title{Compute and plot a Minimum Convex Polygon (MCP)}
\description{The geographical extent of a set of points on a Cartesian plane can be described using a Minimum Convex Polygon (MCP).  The MCP is the minimum area polygon containing a set of point locations.}
\usage{calc_mcp(id=1, destmat = activities, filename="MCP_Output.txt", verbose = FALSE, pct = 100, plot = TRUE, plotdest = TRUE)}
\arguments{
  \item{id}{Provide a unique integer to identify this MCP from others you may construct with other data points}
  \item{destmat}{Two-column matrix or data frame of point coordinates}
  \item{filename}{A character name for an ASCII output file}
  \item{verbose}{Boolean: set to TRUE if extended processing feedback is wanted }
  \item{pct}{Integer 0 <= pct <=100, the percentage of the MCP for which area is provided}
  \item{plot}{Boolean: the MCP will be plotted if set to TRUE}
  \item{plotdest}{Boolean: all points will be plotted if set to TRUE}
}
\details{This function is most powerful when used repetitively within a loop to compute the MCP for subsets of points stored in a large data table.}
\value{
  The result is a LIST
  \item{MCP.area}{The area of the MCP in hectares}
  \item{MCP.pct}{The desired percentage of the MCP for which area is computed}
  \item{MCP.coords}{A matrix containing MCP vertices.  Each row represents a unique point, the first column contains x-coordinates, and the second, y-coordinates}
}
\references{Builds upon MCP functions available in the adehabitat package}
\author{Tarmo K. Remmel}
\note{This function can be used on its own (once) or repetitively in a loop to process grouped point data stored in a larger table.  When used repetitively, be sure to increment the id parameter to ensure that each MCP has a unique identifier.  The output ASCII coordinate file can be further processed using the makeshapes function to generate an ESRI Shapefile for MCP polygons.}
\seealso{
  \code{\link[adehabitat]{mcp}},
  \code{\link{calc_sdd}},
  \code{\link{calc_sde}},
  \code{\link{makeshapes}}
} 
\examples{
  plot.new()
  calc_mcp(id=1, destmat = activities, filename="MCP_Output.txt", verbose = FALSE, pct = 100, plot = TRUE, plotdest = TRUE) 
}
\keyword{arith}
