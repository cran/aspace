\name{acos_d}
\alias{acos_d}
\title{Compute inverse cosine with angle given in degrees}
\description{Provides the functionality of acos, but for input angles measured in degrees (not radians).}
\usage{acos_d(theta = 0)}
\arguments{
  \item{theta}{A numeric angular measurement in degrees from north.}
}
\details{Since the R default is to compute trigonometric functions on angular measurements stored in radians, this simple function performs the conversion from degrees, reducing the need to do so a priori, outside the function.}
\value{Returns a numeric value for the inverse cosine of the specified angular measurement}
\author{Tarmo K. Remmel}
\note{
  To reduce the need for unit conversions prior to calling trigonometric functions, this function accepts input in angular degrees rather than radians.  Depending on the data source, this function may be preferred to the existing version requiring input in angular radians.}
\seealso{ 
  \code{\link{sin_d}}, \code{\link{cos_d}}, 
  \code{\link{tan_d}}, \code{\link{asin_d}}, 
  \code{\link{atan_d}}
}
\examples{acos_d(theta = 90)}
\keyword{array}
