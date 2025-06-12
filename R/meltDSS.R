meltDSS <- function(...){
 argList <- list(...)
 myEnv <- parent.frame()
 argList <- sapply(names(argList), function(x){
  ret <- .decode.arg(argList[[x]])
  if(x %in% c('data')){
   ret <- as.symbol(ret)
  }
  return(ret)
 }, simplify = FALSE)
 out <- do.call(reshape2::melt, argList, envir = myEnv)
 return(out)
}
