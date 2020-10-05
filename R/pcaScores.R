pcaScores <- function(x, rotation, center = TRUE, scale = FALSE, na.rm = FALSE, means = NULL, sds = NULL, lam = 1){

  if (is.data.frame(x)){
    #deal with nas:
    x <- x[complete.cases(x),]
    y <- .split.numerics(x)

    z <- as.matrix(y$numerics)
  } else if(is.matrix(x)) {
    #deal with nas:
    x <- x[complete.cases(x),]
    z <- x
  } else {
    stop("'x' must be a matrix or a data frame")
  }

  if(center){
    means <- .decode.arg(means)

    if(is.null(means)){
      # no global means, we return the local term
      means <- colMeans(z, na.rm = na.rm)
    }
    if (length(means) != ncol(z)){
      stop("length of the means vector must equal the number of columns in 'x'")
    }
    z <- sweep(z, 2, means, '-')
  }

  if(scale){
    sds <- .decode.arg(sds)

    if(is.null(sds)){
      sds <- apply(z,2, function(x) sd(z, na.rm=TRUE) )
    }
    if (length(sds) != ncol(x)){
      stop("length of the standard deviations vector must equal the number of columns in 'x'")
    }
    z <- sweep(z,2, sds, '/')
  }

  scores <- z %*% .decode.arg(rotation, simplifyMatrix = TRUE)
  lam <- .decode.arg(lam)
  if(any(lam != 1)){
    scores <- t(t(scores)/lam) # for biplot
  }
  scores <- as.data.frame(scores)
  colnames(scores) <- sub('V', 'Comp.', colnames(scores))
  # paste back the non numerics for later categorization
  cbind(scores, y$others)
}
