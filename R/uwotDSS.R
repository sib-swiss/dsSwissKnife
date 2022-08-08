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
  mod <- do.call(eval(parse(text=paste0('uwot::', func))), arglist)
  fname <- tempfile(pattern='uwotDSS', tmpdir = tempdir(check=TRUE))

  uwot::save_uwot(mod, fname, unload = TRUE)
  # get the size in order to read it back
  sz <- file.info(fname)$size
  # read and return it
  res  <- readBin(fname, 'raw', n=sz)
#  return(fname)
  return(res)
}


