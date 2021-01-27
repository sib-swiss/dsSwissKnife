VIMDSS <- function(func, arglist, newobj = NULL){
  myparent <- parent.frame()

 .kNN <- function(newobj, ...){
  if(is.null(newobj)){
    newobj <- arglist$x
  }
  myargs <- list(...)
  # get rid of SUBJID in the imputation:
  dfcols <- colnames(myargs$x)
  hidden <- get('hidden', envir = .mycache)
  sapply(c('variable', 'dist_var'), function(p){
    if(is.null(myargs[[p]])){
      myargs[[p]] <<- dfcols
    }
    myargs[[p]] <<- setdiff(myargs[[p]], hidden)
  })
  names(myargs)[names(myargs)=='x'] <- 'data'
  out <- do.call(VIM::kNN, myargs)
  assign(newobj, out, envir = myparent)
  return(TRUE)
 }

 .aggr <- function(...){

  png('temp.png', width = 1024, height = 768)
  #out <- VIM::aggr(df, plot = FALSE)
  out <- VIM::aggr(...)
  #png('temp.png', width = 960, height = 960)
  #myargs <- list(...)
  #myargs$x <- out
  #do.call(plot, myargs)
  dev.off()
  img <- png::readPNG('temp.png')
  file.remove('temp.png')
  out$x <- img
  class(out) <- c('dssaggr', class(out))
  out
 }
 dispatcher <- list(
   kNN = function(...) .kNN(newobj,...),
   aggr = .aggr
 )

 func <- .decode.arg(func)
 arglist <- .decode.arg(arglist)

 newobj <- .decode.arg(newobj)
 # replace the name of the data frame with itself:
 names(arglist)[names(arglist)=='data'] <- 'x'
 newArglist <- arglist
 if('x' %in% names(arglist) ){
   newArglist$x <- get(arglist$x, envir = myparent)
 } else {
   newArglist[[1]] <- get(arglist[[1]], envir = myparent )
   names(newArglist)[1] <- names(arglist)[1] <- 'x'

 }

 if(!(func %in% names(dispatcher))){
   stop(paste0(func, ' not implemented yet.'))
 }

 do.call(dispatcher[[func]], newArglist)


}


