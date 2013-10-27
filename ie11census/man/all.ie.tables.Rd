\name{all.ie.tables}
\alias{all.ie.tables}
\title{List all Irish Census tables}
\usage{
  all.ie.tables(df = FALSE)
}
\arguments{
  \item{df}{Should function return a data frame, or just
  print a list of each table's contents}
}
\value{
  If \code{df = FALSE} list of all the Irish Census tables,
  but return \code{NULL}, otherwise return a data frame
  where each row corresponds to a field, with columns for
  \code{theme}, \code{table}, \code{field} and
  \code{longfield}.
}
\description{
  Useful tool to see all fields in all tables in all themes
  of the Irish Census data, or return them as a data frame.
}

