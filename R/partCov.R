#'
#' @title calculates the matrix of sums of products of distance to the global means,
#' @description used as a piece of the global covariance matrix
#' the client sums all these local matrices and divides them by N-1 -> cov matrix
#' @param x a data frame of matrix
#' @param means a vector of global means (one for each dimension)
#' @param na.rm logical remove NAs?
#' @param collist a vector, subset of columns to use
#' @return  a matrix of the sums of products for all pairs of dimensions

#' @export
#'


partCov <- function(x, means = NULL,  collist = NULL, wt = NULL, cor = FALSE, center = TRUE, method = 'unbiased'){


  # returns a matrix of
  #   sums of
  #     products (for each pair of dimensions) of
  #       distances to the global mean (for each dimension)
  # the client will sum these and divide by the respective, global, N-1
  # the client ends up with a global covariance matrix



    if (is.data.frame(x)){

    if(!is.null(collist)){
      collist <- .decode.arg(collist)
    }
    y <- .split.numerics(x, collist)$numerics

    z <- as.matrix(y)
  } else if(is.matrix(x)) {
    z <- x
  } else {
    stop("'x' must be a matrix or a data frame",call. = FALSE)
  }
  # no NAs
  if(anyNA(z)){
    stop("'x' cannot contain NAs", call. = FALSE)
  }
  # must be datashield valid (more than <datashield.privacyLevel> rows):
  if(!.dsBase_isValidDSS(x)){
    stop(paste0('only ', nrow(x), ' rows'))
  }

  # if wt (weights) is not null return cov.wt only on this node
  if(!is.null(wt)){
    wt <- .decode.arg(wt)

    if(length(wt) == 1){ # it's a name in the global env (maybe a data frame column)
      wt <- .betterExtract(wt, startEnv = parent.frame())
    }
    return(cov.wt(z, wt, cor, center, method))
  }

means <- .decode.arg(means)

  if(is.null(means) | length(means) == 0){
    # no global means, we return the local term
    means <- colMeans(z)
  }

  if (length(means) != ncol(z)){
    stop("length of the means vector must equal the number of columns in 'x'", call. = FALSE)
  }

  # subtract the means (we know from above that the dimensions match):
  centered <- sweep(z, 2, means, '-', check.margin = FALSE)
  # go forth and multiply:
  crossprod(centered)
}
