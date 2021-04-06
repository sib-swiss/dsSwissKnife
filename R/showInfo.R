#' @title Show the levels for all factors in a dataframe
#' @description It filters out  a blacklist of columns ('hidden' vector in the .mycache environment)
#' @param df  a dataframe
#' @return levels for all non blacklisted columns
#'
#'@export
showInfo <- function(df, limit.levels = 'FALSE'){

  #just in case... :
  hidden <- get('hidden', envir = .mycache)
  cols <- colnames(df)[!(colnames(df) %in% hidden)]

  if(length(cols) == 0){
    return(NULL)
  }
  df <- makeFactorsDSS(df)
  cols <- unlist(sapply(cols, function(x) if(is.factor(df[,x])) x))
  if (length(cols) == 0){
    return(NULL)
  }

  # if we are left with a single column, make sure we still describe a data frame:
  df <- as.data.frame(df[,cols])
  colnames(df) <- cols

  sapply(df, showLevels, limit.levels, simplify = FALSE)
}


#' @title Show the levels in a factor
#' @description A safer replacement for levels(). It should replace the definition of the 'levels' method in opal.
#' @param x, a factor
#' @return levels
#'
#'@export

showLevels <- function(x, limit.levels = 'FALSE'){
  # show levels only if not too disclosive
  if (!is.factor(x) || (x %in% get('hidden', envir = .mycache))){
    return(NULL)
  }
  lev <-  levels(x)
  limit.levels <- as.logical(limit.levels)
  if(limit.levels){
    if (length(lev) > 50){
      lev <- lev[1:50]
    }
  }
  lev
}

