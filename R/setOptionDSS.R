#' @export
setOptionDSS<- function(opts){
  opts <- .decode.arg(opts)
  options(opts)
  return(TRUE)
}

getOptionDSS <- function(opts = NULL){
  opts <- .decode.arg(opts)
  if(is.null(opts)){
    return(options())
  }
  sapply(opts, options, USE.NAMES = FALSE)
}
