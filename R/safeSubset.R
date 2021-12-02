#' @title subset a dataframe
#' @description takes potentially unsafe row and column filters and evals them in the context of the dataframe.
#' It takes the following precautions:
#' * it takes a snapshot of all the envirnoments before the execution and deletes any new objects it finds after the execution
#' * it locks all environments before the execution, so no existing object can be modified
#' @param df a dataframe
#' @param row.filter a character, the filter for rows (as in '[')
#' @param col.filter a character, the filter for columns (as in '[')
#' @export


safeSubset <- function(df, row.filter = TRUE, col.filter = TRUE){
  myenv <- parent.frame()
  stopifnot(is.data.frame(df))
  row.filter <- .decode.arg(row.filter)
  col.filter <- .decode.arg(col.filter)
  if(is.null(c(row.filter, col.filter))){
    return(df)
  }
  # take a snapshot of the current session
  safe.objs <- .ls.all()
  safe.objs[['.GlobalEnv']] <- setdiff(safe.objs[['.GlobalEnv']], '.Random.seed')  # leave alone .Random.seed for sample()
  # lock everything so no objects can be changed
  .lock.unlock(safe.objs, lockBinding)
  tryCatch({
    #hidden <-  get('hidden', envir = .mycache)
    #removing this restriction for now:
    #mycols <- setdiff(colnames(df), hidden)
    #ret <-  df[eval(parse(text = row.filter), envir = df[,mycols], enclos = .GlobalEnv), eval(parse(text = col.filter), envir = df[,mycols], enclos = .GlobalEnv), drop = FALSE]
    ret <-  df[eval(parse(text = row.filter), envir = df, enclos = myenv), eval(parse(text = col.filter), envir = df, enclos = myenv), drop = FALSE]
    # keep the rownames:
    rown <- row.names(ret)
    # reform the factors with the new levels:
    ret <- as.data.frame(lapply(ret, function(x){
      if (class(x) == 'factor'){
        return(factor(x))
      }
      x
    }))

  },
  error = function(e){
    # if anything happens, unlock and cleanup:
    .lock.unlock(safe.objs, unlockBinding)
    .cleanup(safe.objs)
    stop(e)
  })
  # unlock back everything
  .lock.unlock(safe.objs, unlockBinding)
  # get rid of any sneaky objects that might have been created in the filters as side effects
  .cleanup(safe.objs)
  # basic check:
  if(!.dsBase_isValidDSS(ret)){
    ret[] <- NA
  }
  #set the original rownames
  row.names(ret) <- rown
  ret
}

