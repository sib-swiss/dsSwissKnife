synthpopDSS <- function(data, arglist, newobj = NULL){
  data <- .decode.arg(data)
  arglist <- .decode.arg(arglist)
  min_minbucket <- getOption("datashield.privacyLevel", default = 5)

  for(i in names(arglist)){
    if(grepl('minbucket', i, fixed = TRUE)){
      if(arglist[[i]] > min_minbucket){
        arglist[[i]] <- min_minbucket
      }
    }
  }

  do.call(synthpop::syn, arglist)
}


