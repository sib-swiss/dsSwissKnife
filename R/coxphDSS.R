#' @export
coxphDSS <- function(arglist){
  arglist <- .decode.arg(arglist)
  arglist$formula <- as.formula(arglist[['formula']])
  caller_frame = parent.frame()
  arglist$data <- get(arglist[['data']], envir = caller_frame)

  #sanitize:
  hidden <- get('hidden', envir = .mycache)
  arglist$data <- arglist$data[, setdiff(names(arglist$data), hidden)]
  arglist$x <- FALSE
  arglist$model <- FALSE
  # look for a 'newdata' data frame for the fit
  new.data <- NULL
  if ('new.dataframe' %in% names(arglist)){
    # take a snapshot of the current session
    safe.objs <- .ls.all()
    # lock everything so no objects can be changed
    .lock.unlock(safe.objs, lockBinding)
    tryCatch({
      new.data <- eval(parse(text = paste0("data.frame(", arglist$new.dataframe, ")")), envir = caller_frame)
    },  error = function(e){
      # if anything happens, unlock and cleanup:
      .lock.unlock(safe.objs, unlockBinding)
      .cleanup(safe.objs)
      stop(e)
    })
    # unlock back everything
    .lock.unlock(safe.objs, unlockBinding)
    # get rid of any sneaky objects that might have been created  as side effects
    .cleanup(safe.objs)
    arglist[['new.dataframe']]<- NULL
  }

  ret <- list()
  ret$model <- do.call(survival::coxph, arglist)

  if(is.null(new.data)){
    ret$fit <- survival::survfit(ret$model)
  } else {
    ret$fit <- survival::survfit(ret$model, newdata = new.data)
  }
  # stuff is disclosed in the call so get rid of it:
  ret$model[['call']] <- NULL
  # residuals too:
  ret$model[['residuals']] <- NULL
  ret
}
