
deriveColumn <- function(x, colname, formula){
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
    culprits <- setdiff(funcs, c(allowed.funcs, 'egfr', 'one.versus.others')) # allow these 2 by default
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
