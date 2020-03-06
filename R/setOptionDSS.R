#' @export
setOptionDSS<- function(opts){
  opts <- .decode.arg(opts)
  options(opts)
  return(TRUE)
}
