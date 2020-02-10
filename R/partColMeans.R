#'
#' @title returns colmeans for this dataset
#' @description  the client can use this function to calculate the global colmeans
#' @param x a dataframe
#' @param na.rm logical remove NAs?
#' @param collist a vector, subset of columns to use
#' @return  a numeric vector which contains the minimum and the maximum
#' @export
#'

partColMeans <- function(x, na.rm = FALSE, collist = NULL){
  
  # must be datashield valid (more than <datashield.privacyLevel> rows):
  if(!dsBase_isValidDS(x)){
    return(NA)
  }
  # be nice and quiet; if no collist has been provided we return the means of the numeric ones
  if(!is.null(collist)){
    collist <- .decode.arg(collist)
  }
  y <- .split.numerics(x, collist)$numerics
  nas <- apply(y, 2, dsBase_numNaDS)
  
  return(list(means = colMeans(y, na.rm = na.rm), nrows = NROW(x), nas = nas, numeric.cols = names(y)))
  
}
