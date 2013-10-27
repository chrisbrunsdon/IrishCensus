.onLoad <- function(libname, pkgname) {
  data("irishcensus2",package=pkgname,envir=parent.env(environment()))
}
