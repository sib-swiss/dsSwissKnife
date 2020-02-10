#' @export
rbindDS <- function(x,y, new.colnames = NULL){
  new.colnames <- .decode.arg(new.colnames)
  if (!is.null(new.colnames)){
    colnames(x) <- new.colnames
    colnames(y) <- new.colnames
  }

  ret <- rbind(x,y)
  if(!dsBase_isValidDS(ret)){
    ret <- ret[NA, NA]
  }
  ret
}
