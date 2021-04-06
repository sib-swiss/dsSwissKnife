#' @title returns the weights of a neural network model trained on client data
#' @description compiles and fits a keras/tensorflow neural network on client
#' @param x string of a dataframe name
#' @param model json string of tensorflow/keras neural network model
#' @param cl.labels string with name of class variable
#' @param compile.args list with arguments passed to compile function
#' @param fit.args list with arguments passed to fit function
#' @export

tfNNFit <- function(x, model, cl.labels, compile.args, fit.args, weights.args = NULL){

  #devtools::load_all()
  x <- get(x, envir = parent.frame())

  model <- keras::model_from_json(.decode.arg(model))

  compile.args <- .decode.arg(compile.args)
  fit.args <- .decode.arg(fit.args)

  # must be datashield valid (more than <datashield.privacyLevel> rows):
  if(!.dsBase_isValidDSS(x)){
    return(NA)
  }

  compile.args <- c(list(model), compile.args)
  compile.args$optimizer <- do.call (.makeOptimizer,as.list(compile.args$optimizer))
  do.call(keras::compile, compile.args)

  if (!is.null(weights.args)) {
    w <- .decode.arg(weights.args)
    set_weights(model, w)
  }

  n <- dim(x)[1]

  lbl <- as.integer(x[[cl.labels]])-1
  flat_labels = to_categorical(as.integer(lbl), num_classes = nlevels(lbl))
  x <- .split.numerics(x)$numerics

#  model <- compile.args[[1]]
  fit.args <- c(list(model), list(as.matrix(x)), list(flat_labels), fit.args)
  do.call(fit, fit.args)

  weights <- get_weights(model)

  return(weights)

}

# Iulian:
# create an optmizerfrom a list of the form
# list(opmtimizerName = '...',  opmtimizerArgs = list(...))

.makeOptimizer <- function(optimizerName, optimizerArgs = list()){
  library(keras)
  myOptimizer <- ls.str('package:keras', pattern = optimizerName)
  if(length(myOptimizer) == 0){
    stop(paste0('No such compile optimizer: ', optimizerName))
  }
  do.call(myOptimizer, optimizerArgs)

}
