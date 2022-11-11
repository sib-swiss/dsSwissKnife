uwotDSS <- function(func, X, arglist = list(), model = NULL){
  if(!(func %in% c('umap', 'umap_transform'))){
    stop(paste0(func, ' not implemented.'))
  }
  X <- .split.numerics(X)$numerics
  arglist <- .decode.arg(arglist)
  arglist$X <- X
  # some sanitizing:
  arglist$ret_nn = FALSE
  arglist$ret_model = FALSE # no full model back
  nms <- names(arglist)
  if('ret_extra' %in% nms){
    arglist$ret_extra < intersect(arglist$ret_extra, 'fgraph')
  }
  if(!is.null(model)){ # this is a path to the saved model
    model <- .decode.arg(model)
    if(model == 'uwot_model'){
      writeBin(get('uwot_model', envir = parent.frame()), 'uwot_model', useBytes = TRUE)  # uwot_model has been uploaded in this session, it's the binary representation of a saved model
    }
    arglist$model <- uwot::load_uwot(model)
  }

  res <- do.call(eval(parse(text=paste0('uwot::', func))), arglist)
#  if(func == 'umap'){ # for the model we have to create and return a blob
#    fname <- tempfile(pattern='uwotDSS', tmpdir = tempdir(check=TRUE))
#    uwot::save_uwot(res, fname, unload = TRUE)
#    # get the size in order to read it back
#    sz <- file.info(fname)$size
#    # read and return it
#    res  <- readBin(fname, 'raw', n=sz)
#  }

  return(res)
}


