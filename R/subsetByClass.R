subsetByClass <- function(first.arg, cols, keep.cols = TRUE){
  first.arg <- .decode.arg(first.arg)
  cols <- .decode.arg(cols)

  objs <- .dsBase_extract(cols)
  call_env <- parent.frame()
  categories <- data.frame(Map(function(x, y){
    if(is.na(y)){
      myenv <- call_env
    } else {
      theenv <- call_env
      myenv <- as.environment(get(y, envir = theenv))
    }
    get(x, myenv)
  }, objs$elements, objs$holders))



  #  real.args <- sapply(first.arg, function(x){
  #    ret <-  get(x, envir = .GlobalEnv)

  #    if(!is.data.frame(ret)){
  #      tmp <- list()
  #      tmp[[x]] <- ret
  #      ret <- tmp
  #    }
  #    ret
  #    }, simplify = FALSE)

  argobjs <- .dsBase_extract(first.arg)

  real.args<- Map(function(x, y){
    if(is.na(y)){
      myenv <- call_env
      ret <- get(x, myenv)
    } else {
      theenv <- call_env
      myenv <- as.environment(get(y, envir = theenv))
      ret <- get(x, myenv)
    }

    if(!is.data.frame(ret)){
      tmp <- list()
      tmp[[x]] <- ret
      ret <- tmp
    }
    ret
  }, argobjs$elements, argobjs$holders)




  real.args.df <- data.frame(real.args)


  colnames(real.args.df) <- unlist(sapply(real.args, names, simplify = FALSE))

  categories <- categories[,!(names(categories) %in% names(real.args.df) ),drop = FALSE]

  real.args.df <- cbind(real.args.df, categories , deparse.level = 2)
  .subsetByClass.df(real.args.df, cols = objs$elements, keep.cols = keep.cols)
}



.subsetByClass.df <- function(df, cols, keep.cols = TRUE){
  #df <- get(data, envir = .GlobalEnv)
  # must be factors. If any of the cols isn't, complain.
  if(!all(sapply(df[, cols], is.factor))){
    stop('The subset column must be a factor.')
  }

  ret <- split(df[,keep.cols, drop = FALSE], df[,cols])

  names(ret) <- make.names(names(ret))
  out <- lapply(ret, function(x){
    if (nrow(x) < .dsBase_setFilterDSS()){
      return(NULL)
    }
    as.data.frame(lapply(x, function(y){ # reset the factor levels
      if(is.factor(y)){
        return(factor(y))
      }
      y
    }))

  })
  #return only the meaningful part:
  out[!sapply(out, is.null)]
}
