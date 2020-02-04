


snf <- function(fn, objs, further.args, action = 'keep', keep.name=NULL){

  action <- .decode.arg(action)
  #check what we are allowed to execute
  if(!(fn %in% c('standardNormalization','dist2','affinityMatrix', 'spectralClustering', 'SNF', 'rankFeaturesByNMI'))){
    stop(paste0('Function ', fn, ' not implemented yet.'))
  }
  # check what we are allowed to return:
  if(fn %in% c('standardNormalization')){
    action <- setdiff(action, 'return')
  }
  if (length(action) == 0){
    return('Return not allowed here, please specify keep.')
  }
  objs <- .decode.arg(objs)
  further.args <- .decode.arg(further.args)
  what <- get(fn, asNamespace('SNFtool'))
  real.objs <- lapply(objs, function(x){
    out <- get(x, envir = parent.frame())
    out
  })

  if(fn == 'standardNormalization'){
    if(!('SUBJID' %in% colnames(real.objs[[1]]))){
      stop('The input object must contain the SUBJID column.')
    }
    row.names(real.objs[[1]]) <- real.objs[[1]]$SUBJID
    real.objs[[1]] <- real.objs[[1]][,setdiff(colnames(real.objs[[1]]), 'SUBJID')]
    real.objs[[1]] <- t(real.objs[[1]])
  }

  if(fn %in% c('SNF', 'rankFeaturesByNMI')){
    real.objs <- list(real.objs)
  }

  if (fn == 'rankFeaturesByNMI'){
    further.args <- list(get(unlist(further.args, recursive = FALSE), envir = parent.frame()))
  }

  myargs <- as.list(append(further.args, real.objs, after = 0))
  ret <- do.call(what, myargs)
  if('keep' %in% action){
    if(is.null(keep.name)){
      keep.name <- paste0(objs[1], '_', fn)
      warning(paste0('The result is stored in ', keep.name), call. = FALSE)
    }
    if(fn == 'spectralClustering'){
      # allow download
      attr(ret, 'download_allowed') <- TRUE
    }
    assign(keep.name, ret, envir = parent.frame())
  }
  if('return' %in% action){
    attr(ret, 'download_allowed') <- NULL # reset attributes
    return(ret)
  }
  return(TRUE)
}


t <- function(x){
  base::t(x)
}
