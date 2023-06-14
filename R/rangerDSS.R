rangerDSS <- function(func, arglist, newobj = NULL){
  myparent <- parent.frame()

 .ranger <- function(...){

  myargs <- list(...)


  do.call(ranger::ranger, myargs)
 }



 dispatcher <- list(
   ranger = function(...) do.call(ranger::ranger, list(...)),
   predict = function(...) do.call(predict, list(...))
 )

 func <- .decode.arg(func)
 arglist <- .decode.arg(arglist)
 if(!(func %in% names(dispatcher))){
   stop(paste0(func, ' not implemented yet.'))
 }
 if(exists('data', where = arglist)){
   arglist$data <- .betterExtract(arglist$data, startEnv = myparent)
 }
 if(exists('x', where = arglist)){
   l <- sapply(arglist$x, function(a) .betterExtract(a, startEnv = myparent), simplify = FALSE)
   arglist$x <-  as.data.frame(Reduce(cbind, l))
 }
 if(exists('y', where = arglist)){
   arglist$y <- .betterExtract(arglist$y, startEnv = myparent)
 }
 if(exists('object', where = arglist)){
   arglist$object <- .betterExtract(arglist$object, startEnv = myparent)
 }

 out <- do.call(dispatcher[[func]], arglist)
 out$call <- NULL
 newobj <- .decode.arg(newobj)
 if(!is.null(newobj)){
   assign(newobj, out, envir = myparent)
 }

}


