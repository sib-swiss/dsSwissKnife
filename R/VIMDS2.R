VIMDS2 <- function(func, arglist, newobj = NULL){
  dispatcher <- list(
    kNN = function(...) .kNN(newobj,...),
    aggr = .aggr
  )
  func <- .decode.arg(func)
  arglist <- .decode.arg(arglist)
  newobj <- .decode.arg(newobj)
  if(!(func %in% names(dispatcher))){
    stop(paste0(func, ' not implemented yet.'))
  }
  do.call(dispatcher[[func]], arglist)
}


.kNN <- function(newobj, data, ...){
  df <- get(data, envir = parent.frame())
  if(is.null(newobj)){
    newobj <- data
  }
  myargs <- list(...)
  # get rid of SUBJID in the imputation:
  dfcols <- colnames(df)
  hidden <- get('hidden', envir = .mycache)
  sapply(c('variable', 'dist_var'), function(p){
    if(is.null(myargs[[p]])){
      myargs[[p]] <<- dfcols
    }
    myargs[[p]] <<- setdiff(myargs[[p]], hidden)
  })
  myargs$data <- df
  out <- do.call(VIM::kNN, myargs)
  assign(newobj, out, envir = parent.frame())
  return(TRUE)
}

.aggr <- function( x, ...){
  df <- get(x, envir =  parent.frame())
  png('temp.png', width = 960, height = 960)
  #out <- VIM::aggr(df, plot = FALSE)
  out <- VIM::aggr(df,...)
  #png('temp.png', width = 960, height = 960)
  #myargs <- list(...)
  #myargs$x <- out
  #do.call(plot, myargs)
  dev.off()
  img <- png::readPNG('temp.png')
  file.remove('temp.png')
  out$x <- img
  class(out) <- c('dss.aggr', class(out))
  out
}
