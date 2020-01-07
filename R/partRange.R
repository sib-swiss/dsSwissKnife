#'
#' @title returns the minimum and maximum of a numeric vector or numeric columns of a data frame
#' @description this function is similar to R function \code{range} but instead to not return
#' the real minimum and maximum, the computed values are multiplied by a very small random number.
#' @param x a numerical or a data frame
#' @return  a numeric vector which contains the minimum and the maximum or a data frame with such
#' vectors as columns
#' @export
#'
partRange <- function(...){


  #Iulian: a modified version of rangeDS from datashield,
  #
  #ex <- .extract(c(...))
  where <- parent.frame()
  ex <- do.call(.extract, list(...), envir = where)
  # Iulian: add handling of an entire data frame
  sapply(ex, function(x){
    if(is.data.frame(x)){
      print(str(x))
      output <- sapply(.split.numerics(x)$numerics, .sloppy.range, simplify = FALSE, USE.NAMES = TRUE)
      return(output)
    }
    if(!(is.numeric(x))){
      output <- NULL
    }else{
      output <-  .sloppy.range(x)
    }
    return (output)
  }, simplify = FALSE)
}

.sloppy.range <- function(x){
  rr <- c(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
  rr <- range(x, na.rm=TRUE)
  random1 <- runif(1, 0.95, 1)
  random2 <- runif(1, 1, 1.05)
  c(rr[1]*random1, rr[2]*random2)
}

