HmiscDSS <- function(func, arglist, newobj){
  if(!'Hmisc' %in% installed.packages()[, 'Package']){
    stop('Package Hmisc is not installed.')
  }
  myenv <- parent.frame()
  force(myenv)
  dispatcher <- list(
    rcorrcens= function(...) {
      a <- list(...)
      a[[1]] <- as.formula(a[[1]])
      names(a)[1] <- 'formula'
      if('data' %in% names(a)){
        a[['data']] <- get(a[['data']], envir = myenv)
      }

    do.call(Hmisc:::rcorrcens.formula,a)

  }
 )
  func <- .decode.arg(func)
  arglist <- .decode.arg(arglist)
  newobj <- .decode.arg(newobj)
  if(!(func %in% names(dispatcher))){
    stop(paste0(func, ' not implemented yet.'))
  }
  do.call(dispatcher[[func]], arglist)
}
