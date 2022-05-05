DDRTreeDSS <- function(func, arglist){
  if(!'DDRTree' %in% installed.packages()[, 'Package']){
    stop('Package DDRTree is not installed.')
  }
  require(DDRTree) # otherwise it fails weirdly
  myenv <- parent.frame()
  force(myenv)
  dispatcher <- list(
    DDRTree = function(...) {
      a <- list(...)
      df <- get(a[[1]], envir = myenv)
      df <- .split.numerics(df)$numerics
      X <- as.matrix(t(df))
      a[[1]] <- X

      res <- suppressWarnings(do.call(DDRTree::DDRTree,a))
      res$X <- NULL # original data was here
      res
    }
  )
  func <- .decode.arg(func)
  arglist <- .decode.arg(arglist)

  if(!(func %in% names(dispatcher))){
    stop(paste0(func, ' not implemented yet.'))
  }
  do.call(dispatcher[[func]], arglist)
}
