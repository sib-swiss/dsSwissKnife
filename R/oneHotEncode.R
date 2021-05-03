#TODO


oneHotEncode <- function(x){
  lvs <- levels(as.factor(x))
  as.data.frame(sapply(lvs, function(y){
    y <- as.numeric(y)
    out <- sign((y+1-x)*(x+1-y))
    out*(out+1)/2
  }, simplify = FALSE))
}
