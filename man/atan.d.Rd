\name{atan.d}
\alias{atan.d}
\title{Compute inverse tangent with angle given in degrees}
\description{Provides the functionality of atan, but for input angles measured in degrees (not radians).} 
\usage{atan.d(theta = 0)}
\arguments{
  \item{theta}{A numeric angular measurement in degrees from north.}
}
\details{Since the R default is to compute trigonometric functions on angular measurements stored in radians, this simple function performs the conversion from degrees, reducing the need to do so a priori, outside the function.}
\value{Returns a numeric value for the inverse tangent of the specified angular measurement.}
\author{Tarmo K. Remmel}
\note{To reduce the need for unit conversions prior to calling trigonometric functions, this function accepts input in angular degrees rather than radians.  Depending on data, this function may be preferred to the existing version requiring input in angular radians.}
\seealso{ 
  \code{\link{sin.d}}, \code{\link{cos.d}}, 
  \code{\link{tan.d}}, \code{\link{asin.d}},
  \code{\link{acos.d}}
}
\examples{atan.d(theta = 90)}
\keyword{array}
