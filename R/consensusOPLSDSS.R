consensusOPLSDSS <- function(arglist){
  myparent <- parent.frame()

 arglist <- .decode.arg(arglist)
 if(exists('data', where = arglist)){
   arglist$data <- .betterExtract(arglist$data, startEnv = myparent)
 }

  if(exists('Y', where = arglist)){
   arglist$Y<- .betterExtract(arglist$Y, startEnv = myparent)
 }
  out <- do.call(ConsensusOPLS::ConsensusOPLS, arglist)
  # out$call <- NULL
 out$optimal <- out$optimal[c('modelCV', 'VIP')]
 out$permuted <- lapply(out$permuted, function(x) x[c('modelCV', 'VIP')])
 return(out)
}


