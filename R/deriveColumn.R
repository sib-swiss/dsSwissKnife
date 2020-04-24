
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
    culprits <- setdiff(funcs, allowed.funcs)
    if(length(culprits) > 0){
      # some illegal functions in there
      stop(paste(culprits, collapse=', '), ' not allowed here. For a list of allowed functions see the documentation.')
    }

  }
  return(parsed)
}



