oneHotDSS <- function(x){
  lvs <- levels(as.factor(x))
  as.data.frame(sapply(lvs, function(y){
    z <- unclass(x[x==y])[1]
    x <- unclass(x)
    out <- sign((z+1-x)*(x+1-z))
    out*(out+1)/2
  }, simplify = FALSE))
}
