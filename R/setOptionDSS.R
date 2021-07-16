#' @export
setOptionDSS<- function(opts){
  opts <- .decode.arg(opts)
  # ensure no setting of critical options:
  nono <- get('forbidden.options', envir = .mycache) # forbidden.options set in .init
  yesyes <- setdiff(names(opts), nono)
  options(opts[yesyes])
  return(nono)
}

getOptionDSS <- function(opts = NULL){
  opts <- .decode.arg(opts)
  if(is.null(opts)){
    return(options())
  }
  sapply(opts, options, USE.NAMES = FALSE)
}
