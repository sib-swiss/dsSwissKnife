uwotDSS <- function(func, X, arglist = list(), model = NULL){
  if(!(func %in% c('umap', 'umap_transform'))){
    stop(paste0(func, ' not implemented.'))
  }
  X <- .split.numerics(X)$numerics
  arglist <- .decode.arg(arglist)
  arglist$X <- X
  # some sanitizing:
  arglist$ret_nn = FALSE
  arglist$ret_extra = c()

  if(!is.null(model)){ # this is a path to the saved model
    arglist$model <- uwot::load_uwot(model)
  }
  res <- do.call(eval(parse(text=paste0('uwot::', func))), arglist)
  fname <- paste0(getwd(),'uwotDSS_model')

  uwot::save_uwot(res, fname, unload = TRUE)
  return(fname)
}


