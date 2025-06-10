removeOutliersDSS <- function(symbol, collist = NULL, sigmas = NULL, maxval = Inf, minval = -Inf){

 if(is.null(collist)){
   collist <- colnames(symbol)
 }
  n_cols <- length(collist)
  if (length(minval) == 1) {
    minval <- rep(minval, n_cols)
  }

  if (length(maxval) == 1) {
    maxval <- rep(maxval, n_cols)
  }
  num <-.split.numerics(symbol)$numerics
 names(minval) <- names(maxval) <- colnames(num)

 as.data.frame(sapply(collist, function(x){
   this.col <- symbol[[x]]
   if(!is.numeric(this.col)){
     return(this.col)
   }
   if(!is.null(sigmas)){
     sigma <- sd(this.col, na.rm = TRUE)
     m <- mean(this.col, na.rm = TRUE)
     minval[x] <-  m - sigmas*sigma
     maxval[x] <-  m + sigmas*sigma
   }
   this.col[this.col < minval[x] | this.col > maxval[x]] <- NA
   return(this.col)
 }, simplify = FALSE))
}
