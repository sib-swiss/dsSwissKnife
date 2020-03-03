
fullData <- function(what, package = NULL){

  data(list = what, package = package, envir=parent.frame())

  TRUE
}
