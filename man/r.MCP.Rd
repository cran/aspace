\name{r.MCP}
\alias{r.MCP}
\docType{data}
\title{Demo Data: Minimum Convex Polygon Output Object}
\description{Results from the Minimum Convex Polygon Calculator (calc_mcp) are stored in a list object. This object is required for the plot function (plot_mcp).}
\usage{data(r.MCP)}
\format{
  The list object contains the following results:
  \describe{
    \item{MCP}{A matrix containing MCP vertices.  Each row represents a unique point, the first column contains x-coordinates, and the second, y-coordinates}
    \item{points}{a simple two-column data frame (or matrix) containing x,y coordinates for a series of point locations.}
    \item{id}{Identifier for the MCP}
    \item{MCP.area}{The area of the MCP in square kilometers}
    \item{MCP.pct}{The desired percentage of the MCP for which area is computed}
  }
}
\details{The coordinates of the points must have the same units and projection as the specified center.}
\source{This demonstration data has been manufactured for illustrative purposes only.}
\examples{
  data(r.MCP)
  str(r.MCP)
}
\keyword{datasets}