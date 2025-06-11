removeOutliersDSS <- function(symbol, collist = NULL, sigmas = NULL, maxval = NULL, minval = NULL){
 if(is.null(maxval)){
   maxval <- Inf
 }
 if(is.null(minval)){
   minval <- -Inf
 }

 num <-.split.numerics(symbol)$numerics
 if(is.null(collist)){
   collist <- colnames(num)
 }
  n_cols <- length(collist)
  if (length(minval) == 1) {
    minval <- rep(minval, n_cols)
  }

  if (length(maxval) == 1) {
    maxval <- rep(maxval, n_cols)
  }

 names(minval) <- names(maxval) <- collist

 as.data.frame(sapply(colnames(symbol), function(x){
   this.col <- symbol[[x]]
   if(!(x %in% collist)){
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
