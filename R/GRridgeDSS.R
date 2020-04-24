GRridgeDSS <- function(func, arglist, newobj){
  myenv <- parent.frame()
  force(myenv)
  dispatcher <- list(
    CreatePartition= function(...) {
      a <- list(...)
      a[[1]] <- get(a[[1]], envir = myenv)

      capture.output(part <- do.call(GRridge::CreatePartition,a))
      assign(newobj, part, envir = myenv)
      summary(part)[,1, drop = FALSE]
    },
    grridge = function(...){
      a <- list(...)
      for(i in 1:3){
        a[[i]] <- get(a[[i]], envir = myenv)
      }
      capture.output(ret <- do.call(GRridge::grridge,a))
      # sanitize:
      ret[['predobj']][['NoGroups']][['residuals']] <- NULL
      ret[['predobj']][['GroupRegul']][['residuals']] <- NULL
      ret[['arguments']][['partitions']] <- NULL
      ret
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
