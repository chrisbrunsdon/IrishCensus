\name{long.field.names}
\alias{long.field.names}
\title{Return full length field names given short field names for items in the Irish Census}
\usage{
  long.field.names(x)
}
\arguments{
  \item{x}{Short field name in character form (eg
  \code{'T12_21_14M'})}
}
\value{
  Long field decsription (eg \code{'1-14 Hours (Males)')}
}
\description{
  This is a useful tool for creating tables with more 'user
  friendly' column or row labels. The short field codes are
  used to refer to fields in calculations, but are not very
  informative in reported output.  This function addresses
  that issue.
}
\examples{
long.field.names(c('T12_21_14M','T12_215_28M','T12_21_14F','T12_215_28F'))
}

