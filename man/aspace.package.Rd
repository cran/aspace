\name{aspace-package}
\alias{aspace-package}
\alias{aspace}
\docType{package}
\title{A collection of functions for estimating centrographic statistics and computational geometries for spatial point patterns}
\description{A collection of functions for computing centrographic statistics (e.g., standard distance, standard deviation ellipse, standard deviation box) for observations taken at point locations. Separate plotting functions have been developed for each measure. Users interested in writing results to ESRI shapefiles can do so by using results from aspace functions as inputs to the convert.to.shapefile and write.shapefile functions in the shapefiles library. The aspace library was originally conceived to aid in the analysis of spatial patterns of travel behaviour (see Buliung and Remmel, 2008). Major changes in the current version include (1) removal of dependencies on several external libraries (e.g., gpclib, maptools, sp), (2) the separation of plotting and estimation capabilities, (3) reduction in the number of functions, and (4) expansion of analytical capabilities with additional functions for descriptive analysis and visualization (e.g., standard deviation box, centre of minimum distance, central feature).}
\details{
\tabular{ll}{
Package: \tab aspace\cr
Type: \tab Package\cr
Version: \tab 3.2\cr
Date: \tab 2012-08-08\cr
License: \tab GPL (>= 2.0)\cr
}
}
\author{Randy Bui, Ron N. Buliung, Tarmo K. Remmel}
\references{
  Bachi, R. 1963. Standard distance measures and related methods for spatial analysis. 
  Papers of the Regional Science Association 10: 83-132.   
  \cr\cr
  Buliung, R.N. and Remmel, T. (2008) Open source, spatial analysis, and activity travel 
  behaviour research: capabilities of the aspace package. Journal of 
  Geographical Systems, 10: 191-216.
  \cr\cr
  Buliung, R.N. and Kanaroglou, P.S. (2006) Urban form and household activity-travel 
  behaviour. Growth and Change, 37: 174-201.  
  \cr\cr
  Ebdon, D. 1988. Statistics in Geography 2nd Edition. Oxford UK: Blackwell. 
  \cr\cr
  Levine, N. 2002. CrimeStat II: A Spatial Statistics Program for the Analysis 
  of Crime Incident Locations (version 2.0) Houston TX/National Institute of Justice, 
  Washington DC: Ned Levine & Associates.
}
\keyword{arith}