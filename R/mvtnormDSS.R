mvtnormDSS <- function(func, arglist, newobj = NULL){
  myenv <- parent.frame()
  force(myenv)
  dispatcher <- list(
    dmvnorm = function(...) {
      a <- list(...)
      lim <- min(c(3, length(a)))
      a[1:lim] <-  sapply(a[1:lim], function(x){
                                 if(!is.null(x)){
                                  tryCatch(.betterExtract(x, myenv), error = function(e)x)
                                 }
                                }, simplify = FALSE)
      out <- do.call(mvtnorm::dmvnorm,a)
      if(!is.null(newobj)){
        assign(newobj, out, envir = myenv)
      }
    }
  )
  func <- .decode.arg(func)
  arglist <- .decode.arg(arglist)
  if(!(func %in% names(dispatcher))){
    stop(paste0(func, ' not implemented yet.'))
  }
  do.call(dispatcher[[func]], arglist)
}
