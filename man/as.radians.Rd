\name{as.radians}
\alias{as.radians}
\title{Converts degrees to radians}
\description{This function converts an angular measure stored in degrees to radians. This is an alternative to the rad function available in the package circular.}
\usage{as.radians(theta = 0)}
\arguments{
  \item{theta}{A numeric angular measurement in degrees from north.}
}
\details{Achieves a very simple conversion with a convenient function call.}
\value{Returns a numeric value for an angle in radians that is equivalent to the input theta in degrees.}
\author{Tarmo K. Remmel}
\note{The purpose of this function is to reduce computer code clutter when using angular measuremnts in R.  The simple function call ensures that degree to radian conversions are completed consistently and accurately.  Since trigonometric functions in R require angular measures in radians rather than degrees, this simple function can be used for simple angular unit conversion.}
\seealso{
  \code{\link{sin.d}}, \code{\link{cos.d}}, \code{\link{tan.d}}, 
  \code{\link{asin.d}}, \code{\link{acos.d}}, \code{\link{atan.d}}
}
\examples{as.radians(theta = 90)}
\keyword{array}
