synthpopDSS <- function(data, arglist, newobj = NULL){
  data <- .decode.arg(data)
  data <- get(data, envir = parent.frame())
  arglist <- .decode.arg(arglist)
  arglist[['data']] <- data
  min_minbucket <- getOption("datashield.privacyLevel", default = 5)

  for(i in names(arglist)){
    if(grepl('minbucket', i, fixed = TRUE)){
      if(arglist[[i]] > min_minbucket){
        arglist[[i]] <- min_minbucket
      }
    }
  }
  # all the numeric columns should be smoothed:
  sm <- list()
  sm[colnames(data[,sapply(data, is.numeric)])] <- 'density' # all the numeric columns
  if(length(sm) > 0){
    arglist[['smoothing']] <- sm
  }
  arglist[['print.flag']] = FALSE
  # more sanitizing, the drop. args are essential, otherwise it returns the actual data:
  arglist[['drop.not.used']] <- TRUE
  arglist [['drop.pred.only']] <- TRUE
  # models is risky to I think:
  arglist[['models']] <- FALSE
  arglist[['strata']] <- NULL
  do.call(synthpop::syn, arglist)
}


