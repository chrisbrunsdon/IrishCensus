\name{census.ie}
\alias{census.ie}
\title{Main function to extract Irish Census data}
\usage{
  census.ie(theme = NA, table = NA, field = NA,
    longfield = NA, geogtype = "CTY")
}
\arguments{
  \item{theme}{Grep pattern to search for census themes -
  only search if not \code{NA}}

  \item{table}{Grep pattern to search for a census table -
  only search if not \code{NA}}

  \item{field}{Grep pattern to search for a census field -
  only search if not \code{NA}}

  \item{longfield}{Grep pattern to search for a census long
  field - only search if not \code{NA}}

  \item{geogtype}{Geography type for variable to be
  returned}
}
\value{
  An \code{iecens} object containing the data.  This
  inherits from the \code{data.frame} object but has some
  functionality specific to a data chunk extracted from the
  Irish Census.
}
\description{
  Enables data to be extracted from the Irish Census on the
  basis of theme, table, field in table or long field name
}
\details{
  Only one of \code{theme}, \code{table}, \code{field},
  \code{longfield} should be specified.  If none are
  specified, all variables are returned. \code{geogtype}
  must be one of: \itemize{ \item{\code{'CTY'}} Counties
  \item{\code{'DC'}} Dáil Constituencies 2007
  \item{\code{'ED'}} Electoral Divisions \item{\code{'GA'}}
  Gealtacht Areas \item{\code{'LEA'}} Local Electoral Areas
  \item{\code{'LT'}} Legal Towns and Cities
  \item{\code{'NDC'}} Dáil Constituencies 2013
  \item{\code{'PA'}} Dublin Parishes \item{\code{'PR'}}
  Provinces \item{\code{'RA'}} Regional Authority Areas
  \item{\code{'RE'}} Dioceses \item{\code{'SA'}} Small
  Areas \item{\code{'ST'}} Settlements }
}
\examples{
# Find all fields specifying counts of time people leave home for work or school
# with counts per province
leave.time <- census.ie(geog='PR',field='T11_2_T[1-8]')
leave.time
}

