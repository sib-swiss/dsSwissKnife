partKde2d <-function(holder = NULL, x,y,bandwidth, lims , npoints = 128){

  holder <- .decode.arg(holder)

  if(is.null(holder)){ # no categories
   # real.args <- .dsBase_extract(c(x,y))
    real.args <- do.call(.extract, list(c(x,y), start.env = NULL), envir = parent.frame())
    if(length(real.args) == 1){ # plot a column against itself
      real.args[[2]] <- real.args[[1]]
    }

    return(list(total = one.level.kde2d(real.args[[1]],real.args[[2]], bandwidth, lims, npoints)))
  }
  holder <- get(holder, envir = parent.frame())
  r <- sapply(holder, function(this.level){
    real.args <- .extract(c(x,y), start.env = this.level)
    #one.level.kde2d(this.level[[new.x]], this.level[[new.y]], bandwidth, lims, npoints)
    if(length(real.args) == 1){ # plot a column against itself
      real.args[[2]] <- real.args[[1]]
    }
    one.level.kde2d(real.args[[1]], real.args[[2]], bandwidth, lims, npoints)
    #one.level.kde2d(paste0(holder, '$', this.name, '$',new.x), paste0(holder, '$', this.name, '$', new.y), bandwidth, lims, npoints)
  }, simplify = FALSE)

  return(r)
}

# wrapper around kde2d from package MASS
#' @export
one.level.kde2d <- function(x,y,bandwidth , lims  , npoints = 128){

  if(length(x) < 5 || length(y) < 5){
    return(NULL)
  }
  bw.args <- .decode.arg(bandwidth)

  lims <- .decode.arg(lims)

  bw <-c(.tot.bw.nrd(bw.args$x, x),.tot.bw.nrd(bw.args$y, y) )

  k <- .kde2d(x,y, h = bw, n = npoints,lims = unlist(lims))

  return(k)
}

#' bandwidth.nrd
#' From package MASS

.tot.bw.nrd <- function(tot, x){
  # we need to guard against tampering with the parameters on the client side
  # this could result in tiny bandwidths -> no smoothing
  # so we try to provide some sane defaults and then choose the largest value
  r <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  h <- (r[2L] - r[1L])/1.34
  v <- var(x, na.rm = TRUE)/2
  l <- length(x)
  h2 <- (tot$quarts[2L][[1]] - tot$quarts[1L][[1]])/1.34
  h <- max(h, h2)
  epsilon<-1.0e-03
  if(is.na(h) | h<=epsilon){
    h <- epsilon
  }
  v <- max(v, tot$var)
  if(v<=0){
    stop('variance is 0, proceeding would disclose individual data')
  }
  l <- max(l, tot$len)
  out <- 4 * 1.06 * min(sqrt(v), h) * l ^ (-1/5)

  out
}

#' Two-Dimensional Kernel Density Estimation
#' From package MASS


.kde2d <- function(x, y, h, n = 128, lims = unlist(rangeDS2(deparse(substitute(x)), deparse(substitute(y)))) ){
  nx <- length(x)
  if(length(y) != nx)
    stop("data vectors must be the same length")
  if(any(!is.finite(x)) || any(!is.finite(y)))
    stop("missing or infinite values in the data are not allowed")
  if(any(!is.finite(lims)))
    stop("only finite values are allowed in 'lims'")
  n <- rep(n, length.out = 2L)
  # if it's the same column on both axes we have only 2 elements in lims, so
  if(length(lims) == 2){
    lims <- rep(lims, 2)
  }
  gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
  h <- rep(h, length.out = 2L)
  if (any(h <= 0))
    stop("bandwidths must be strictly positive")
  h <- h/4                            # for S's bandwidth scale
  ax <- outer(gx, x, "-" )/h[1L]
  ay <- outer(gy, y, "-" )/h[2L]
  z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), , nx))/ (nx * h[1L] * h[2L])
  list(x = gx, y = gy, z = z, len = nx)
}

