uwotDSS <- function(func, X, arglist){
  X <- .split.numerics(X)$numerics
  arglist <- .decode.arg(arglist)
  arglist$X <- X
  do.call(eval(parse(text=paste0('uwot::', func))), arglist)

}


