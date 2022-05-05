archetypesDSS <- function(func, arglist){
  myEnv <- parent.frame()
  .steparchs <- function(args){
    args[[1]] <- get(args[[1]], envir = myEnv)
    out <- do.call(archetypes::stepArchetypes, args)
    sapply(out, function(x){
      sapply(x, function(y){
        y$residuals <- list()
        y$call <- NULL
        y$history <- NULL
        y
      }, simplify = FALSE)
    }, simplify = FALSE)
   }
 dispatcher = list('stepArchetypes'= .steparchs)
 dispatcher[[func]](arglist)

}
