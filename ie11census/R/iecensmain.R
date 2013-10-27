#' Main function to extract Irish Census data
#' 
#' Enables data to be extracted from the Irish Census on the basis of theme,
#' table,  field in table or long field name
#' 
#' @param theme Grep pattern to search for census themes - only search if not \code{NA}
#' @param table Grep pattern to search for a census table - only search if not \code{NA} 
#' @param field Grep pattern to search for a census field - only search if not \code{NA}
#' @param longfield Grep pattern to search for a census long field - only search if not \code{NA}
#' @param geogtype Geography type for variable to be returned
#' @details Only one of \code{theme}, \code{table}, \code{field}, \code{longfield} should be specified.  If none are specified,  all variables are returned.
#' \code{geogtype} must be one of:
#' \itemize{
#'   \item{\code{'CTY'}}  Counties
#'   \item{\code{'DC'}}  Dáil Constituencies 2007
#'   \item{\code{'ED'}}  Electoral Divisions
#'   \item{\code{'GA'}}  Gealtacht Areas
#'   \item{\code{'LEA'}}  Local Electoral Areas
#'   \item{\code{'LT'}}  Legal Towns and Cities
#'   \item{\code{'NDC'}}  Dáil Constituencies 2013
#'   \item{\code{'PA'}}  Dublin Parishes
#'   \item{\code{'PR'}}  Provinces
#'   \item{\code{'RA'}}  Regional Authority Areas
#'   \item{\code{'RE'}}  Dioceses
#'   \item{\code{'SA'}}  Small Areas
#'   \item{\code{'ST'}}  Settlements
#' }
#' @return An \code{iecens} object containing the data.  This inherits from the \code{data.frame} object but has some functionality specific to a data chunk extracted from the Irish Census.
#' @examples
#' # Find all fields specifying counts of time people leave home for work or school
#' # with counts per province
#' leave.time <- census.ie(geog='PR',field='T11_2_T[1-8]')
#' leave.time
#' @export
census.ie <- function(theme=NA,table=NA,field=NA,longfield=NA,geogtype="CTY") {
  trimmed <- subset(datasets,GEOGTYPE==geogtype)
  if (!is.na(field)) {
    used <- grep(field,attr(datasets,"variables")$field)
    if (length(used) == 0) stop("No themes match this pattern")
    res <- trimmed[,c(1:3,used+3)]
    class(res) <- c('iecens',class(res))
    return(res)} 
  if (!is.na(longfield)) {
    used <- grep(longfield,attr(datasets,"variables")$longfield)
    if (length(used) == 0) stop("No themes match this pattern")
    res <- trimmed[,c(1:3,used+3)]
    class(res) <- c('iecens',class(res))
    return(res)} 
  if (!is.na(theme)) {
    used <- grep(theme,attr(datasets,"variables")$theme)
    if (length(used) == 0) stop("No themes match this pattern")
    res <- trimmed[,c(1:3,used+3)]
    class(res) <- c('iecens',class(res))
    return(res)
  }
  if (!is.na(table)) {
    used <- grep(table,attr(datasets,"variables")$table)
    if (length(used) == 0) stop("No tables match this pattern")
    res <- trimmed[,c(1:3,used+3)]
    class(res) <- c('iecens',class(res))
    return(res)
  }
  res <- trimmed
  class(res) <- c('iecens',class(res))
  return(res)
}

#' Method to print an \code{iecensus} object
#' 
#' @export
print.iecens <- function(x,...) {
  cat("\nIrish Census Data Object\n------------------------\n\n\n")
  cat("Geographical Units: ",attr(datasets,"geographies")[x$GEOGTYPE[1]],"\n")
  cat("Variables Stored:\n")
  print(extract.vars(x))
  cat(sprintf("\n\nVariables (Excl. GEOGID, GEOGTYPE and GEOGDESC): %d; Locations: %d\n\n",ncol(x) - 3,nrow(x)))
  cat("Top left corner:\n\n")
  ccols <- 1:min(ncol(x),10)
  crows <- 1:min(nrow(x),5)
  print.data.frame(head(x[crows,ccols]))
}

#' List all Irish Census tables
#' 
#' Useful tool to see all fields in all tables in all themes of the Irish Census data, or return them as a data frame.
#'
#' @param df Should function return a data frame,  or just print a list of each table's contents
#' @return If \code{df = FALSE} list of all the Irish Census tables, but return \code{NULL}, otherwise return a data frame where each row corresponds to a field,  with columns for \code{theme}, \code{table}, \code{field} and \code{longfield}.
#' @export
all.ie.tables <- function (df=FALSE) {
  if (df) return(attr(datasets,'variables'))
  vars <- attr(datasets,"variables")
  class(vars) <- c("vlist",class(vars))
  print.vlist(vars)}



#' List all tables in a given theme
#'
#' Shows all available tables in a given theme number
#' 
#' @param n Theme number
#' @return Returns \code{NULL} but lists the tables in a given theme
#' @examples 
#' theme.ie.tables(3)
#' @export
theme.ie.tables <- function (n) {
  vars <- attr(datasets,'variables')
  vars <- vars[grep(sprintf('T%d_',n),vars$field),]
  class(vars) <- c("vlist",class(vars))
  print.vlist(vars)}


#' List all themes
#' 
#' Basic list of Irish Census themes - use together with \code{theme.ie.tables} to identify tables of interest
#' 
#' @return Returns \code{NULL} but prints out list of Irish Census themes
#' @examples
#' all.ie.themes()
#' @export
all.ie.themes <- function(x) {
  to.show <- unique(attr(datasets,"variables")$theme)
  for (theme in to.show) cat(theme,'\n')
}

extract.vars <- function(x) {
  cvs <- colnames(x)[-(1:3)]
  vars <- attr(datasets,"variables")
  vars <- vars[match(cvs,vars$field),]
  class(vars) <- c("vlist",class(vars))
  return(vars)}

print.vlist <- function(x,...) {
  cur.theme <- ""
  cur.table <- ""
  for (i in 1:nrow(x)) {
    this.row <- x[i,]
    if (this.row$theme != cur.theme) {
      cur.theme <- this.row$theme
      cat(cur.theme,":\n") }
    if (this.row$table != cur.table) {
      cur.table <- this.row$table
      cat("  ",cur.table,":\n") }
    cat("    ",this.row$longfield,sprintf("(%s)\n",this.row$field))
  }
}

#' Return full length field names given short field names for items in the Irish Census
#' 
#' This is a useful tool for creating tables with more 'user friendly' column or row labels.
#' The short field codes are used to refer to fields in calculations,  but are not very informative in 
#' reported output.  This function addresses that issue.
#' 
#' @param x Short field name in character form (eg \code{'T12_21_14M'})
#' @return Long field decsription (eg \code{'1-14 Hours (Males)')}
#' @examples
#' long.field.names(c('T12_21_14M','T12_215_28M','T12_21_14F','T12_215_28F'))
#' @export
long.field.names <- function(x) {
  vars <- attr(datasets,"variables")
  return(vars$longfield[match(x,vars$field)])
}


#' Compute percentages from tables in the Irish Census
#' 
#' Often it is helpful to express values in particular Irish Census tables as percentages,  when each field is a count
#' of people (or households) falling into a given category.  This can always be achieved via \code{transform} but this function
#' is a convenience tool allowing several columns to be converted to percentages in a single function call
#' 
#' @param x \code{iecens} object containing the fields to convert to percentages
#' @param fc The name of the first field for which a percentage is to be computed
#' @param tc The name of the last field for which a percentage is to be computed
#' @param denom If supplied,  specifies the field used as the denominator in percentage calculation.  If not supplied,  the denominator is the sum of the field specified by \code{tc} and \code{fc}.
#' @return A data frame whose column names are the field names specified by \code{'fc'} and \code{'tc'} - a final column (of type \code{character}) is also added, called \code{GEOGID}, and corresponding to the same name column in \code{x}.
#' @examples
#' # Percentage of Irish speakers/non-speakers amongst those respondants who stated 'yes' or 'no' to the census question, by province
#' speak.irish <- census.ie(table='ability to speak Irish',geog='PR')
#' pct.cols(speak.irish,'T3_1YES','T3_1NO')
#' # The same percentages,  but with a denominator of *all* respondants, including those not stating either 'yes' or 'no'
#' pct.cols(speak.irish,'T3_1YES','T3_1NO',denom='T3_1T')
#' @export
pct.cols <- function(x,fc,tc,denom=NA) {
  if (! ("iecens" %in% class(x))) stop("First argument is not of class \"iecens\"")
  fcl <- match(fc,colnames(x))
  if (is.na(fcl)) stop("Can't find start column")
  tcl <- match(tc,colnames(x))
  if (is.na(tcl)) stop("Can't find end column")  
  col.refs <- colnames(x)[fcl:tcl]
  if (is.na(denom)) {
    res <- x[,col.refs]
    res <- 100*sweep(res,1,rowSums(res),'/')
  } else {
    dcl <- match(denom,colnames(x))
    res <- 100 * x[,col.refs] /x[,dcl]
  }
  class(res) <- "data.frame"
  new.col.refs <- sprintf("%s_PC",col.refs)
  colnames(res) <- new.col.refs
  res$GEOGID <- x$GEOGID
  return(res)
}

