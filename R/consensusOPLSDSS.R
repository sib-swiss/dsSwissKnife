consensusOPLSDSS <- function(func, arglist){
  myparent <- parent.frame()



 dispatcher <- list(
   consensusOPLS = function(...) do.call(ConsensusOPLS::ConsensusOPLS, list(...)),
 )

 func <- .decode.arg(func)
 arglist <- .decode.arg(arglist)
 if(!(func %in% names(dispatcher))){
   stop(paste0(func, ' not implemented yet.'))
 }
 if(exists('data', where = arglist)){
   arglist$data <- .betterExtract(arglist$data, startEnv = myparent)
 }

  if(exists('Y', where = arglist)){
   arglist$y <- .betterExtract(arglist$Y, startEnv = myparent)
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
return(out)
}


