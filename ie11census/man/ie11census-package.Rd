\docType{package}
\name{ie11census-package}
\alias{ie11census-package}
\title{Access to Irish Census 2011}
\description{
  Allows access to the 2011 Irish Census data
}
\details{
  This package contains all of the data for the 2011 Irish
  Census, plus some functions to allow simple access to
  this data.
}
\examples{
leave.time <- census.ie(geog='PR',field='T11_2_T[1-8]')
}
\author{
  Chris Brunsdon
}
\references{
  \url{http://www.cso.ie/en/census/index.html}
}
\keyword{package}

