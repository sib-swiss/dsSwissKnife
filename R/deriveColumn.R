
deriveColumn <- function(x, colname, formula ){

  formula <- .decode.arg(formula)
  parsed <- .parseFormula(formula)
  x[[colname]] <- eval(parsed, envir = x)
  return(x)
}



.parseFormula <- function(f){
  # check formulas use only allowed operations and functions
  parsed <- parse(text = f)
  parsetree <- getParseData(parsed)
  # no square brackets:
  if (any(grepl('[',parsetree$token, fixed = TRUE))){
    stop('Selection of individual elements (with "[") is not permitted.')
  }
  # allowed functions:
  funcs <-  parsetree[parsetree$token == 'SYMBOL_FUNCTION_CALL', 'text']
  if(length(funcs) >0 ){
    allowed.funcs <- get('allowed.funcs', envir = .mycache)
    culprits <- setdiff(funcs, c(allowed.funcs, 'egfr', 'one.versus.others', 'rnorm.0.1', 'mergeConceptIds')) # allow these 4 by default
    if(length(culprits) > 0){
      # some illegal functions in there
      stop(paste(culprits, collapse=', '), ' not allowed here. For a list of allowed functions see the documentation.')
    }

  }
  return(parsed)
}



egfr <- function(scr, sex, age, black = FALSE) {
 # if(length(scr) > 1){
#    stop('Only one measurement of creatinine allowed.')
#  }
  if(tolower(sex) %in% c('m', 'male') ){
    alfa <- -0.411
    k <- 61.9
    coef1 <-  1
  } else if (tolower(sex) %in% c('f', 'female')){
    alfa <- -0.329
    k <- 79.6
    coef1 <- 1.018
  }

  coef2 <- 1
  if(black){
    coef2 <- 1.159
  }

  return(141 * (min(scr/k, 1))^alfa * (max(scr/k, 1))^(-1.209) * 0.993^age * coef1 * coef2)

}

egfr <- function(scr, sex, age, black = FALSE) {
  # vectorized version
  alfa <- c()
  k <- c()
  coef1 <- c()
  males <- which(tolower(sex) %in% c('m', 'male') )
  females <- which(tolower(sex) %in% c('f', 'female') )
  alfa[males] <- -0.411
  k[males] <- 61.9
  coef1[males] <-  1

  alfa[females] <- -0.329
  k[females]<- 79.6
  coef1[females] <- 1.018



  coef2 <- rep(1, length(sex))
  if(length(black) < length(sex)){
    black <- rep(black, length(sex))
  }

  coef2[which(black == TRUE)] <- 1.159

  return(141 * (unlist(lapply(scr/k, min, 1)))^alfa * (unlist(lapply(scr/k,max, 1)))^(-1.209) * 0.993^age * coef1 * coef2)

}

one.versus.others <- function(col, positive.level){
  if(!is.factor(col)){
    stop('The input must be a factor')
  }
  levs <- levels(col)

  negative.level <- paste0('no_', positive.level)
  levels(col) <- c(levels(col), negative.level)
  col[col != positive.level ] <- negative.level
  factor(col)
}

rnorm.0.1 <- function(){
  # on column of standard normal distribution
  n <- nrow(as.data.frame(as.list(parent.frame())))
  rnorm(n,0,1)
}


# ftest <- '(function() nrow(as.data.frame(as.list(parent.frame())))()'

#.mergeTwo <- function(first, second){
#  first[is.na(first)] <- second[is.na(first)]
#  first
#}

#mergeConceptIds <- function(primaryConceptId, dict){
#  myenv <- parent.frame()
#  Reduce(.mergeTwo, data.frame(get(primaryConceptId, envir = myenv), unlist(lapply(dict[[primaryConceptId]], function(x) get(x, envir = myenv)))))
#}

mergeConceptIds <- function(primaryConceptId, dict){
myenv <- parent.frame()
dict <- .betterExtract(dict, .GlobalEnv)
nms <- names(dict)
vals <- Reduce(c, dict)
tot <- c(nms, vals)
if(length(tot) != length(unique(tot))){
  stop('Duplicate names found in the dictionary.')
}
  p <- tryCatch(get(primaryConceptId, envir = myenv), error = function(e){
   someName <- ls(envir = myenv)[1]
   rep(NA, length(myenv[[someName]]))
  })

  for (mycol in  dict[[primaryConceptId]]){
    if(exists(mycol, envir = myenv, inherits = FALSE)){

      myVector <- get(mycol, envir = myenv)
      p[is.na(p)] <- myVector[is.na(p)]
    }
  }
  if(all(is.na(p))){
    p <- NULL
  }
  p
}

