\name{pct.cols}
\alias{pct.cols}
\title{Compute percentages from tables in the Irish Census}
\usage{
  pct.cols(x, fc, tc, denom = NA)
}
\arguments{
  \item{x}{\code{iecens} object containing the fields to
  convert to percentages}

  \item{fc}{The name of the first field for which a
  percentage is to be computed}

  \item{tc}{The name of the last field for which a
  percentage is to be computed}

  \item{denom}{If supplied, specifies the field used as the
  denominator in percentage calculation.  If not supplied,
  the denominator is the sum of the field specified by
  \code{tc} and \code{fc}.}
}
\value{
  A data frame whose column names are the field names
  specified by \code{'fc'} and \code{'tc'} - a final column
  (of type \code{character}) is also added, called
  \code{GEOGID}, and corresponding to the same name column
  in \code{x}.
}
\description{
  Often it is helpful to express values in particular Irish
  Census tables as percentages, when each field is a count
  of people (or households) falling into a given category.
  This can always be achieved via \code{transform} but this
  function is a convenience tool allowing several columns
  to be converted to percentages in a single function call
}
\examples{
# Percentage of Irish speakers/non-speakers amongst those respondants who stated 'yes' or 'no' to the census question, by province
speak.irish <- census.ie(table='ability to speak Irish',geog='PR')
pct.cols(speak.irish,'T3_1YES','T3_1NO')
# The same percentages,  but with a denominator of *all* respondants, including those not stating either 'yes' or 'no'
pct.cols(speak.irish,'T3_1YES','T3_1NO',denom='T3_1T')
}

