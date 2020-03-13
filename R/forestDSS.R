
#' @title Construct of a random forest based on training set `df`.
#' @description Calls `randomForest::randomForest` with formula dep_var ~ expl_vars.
#'   The number of trees produced is proportional to the number of rows in the dataset,
#'   but with a minimum of 10 and a maximum of 150 trees.
#' @return a randomForest object.
#' @param df: [data frame] The training dataset.
#' @param dep_var: [string] the response factor ("y"), i.e. the categories
#'   that will be the leaves of the tree.
#' @param expl_vars: [vector[string]] the classification variables.
#' @param nodesize: minimal number of points in the terminal nodes.
#'    We fix this high enough to ensure more privacy. Default is 1
#'    for classification and 5 for regression.
#' @param ...: other parameters to `randomForest`.
#'
forestDSS <- function(df, dep_var, expl_vars, nodesize = 5, ...) {
  dep_var <- .decode.arg(dep_var)
  expl_vars <- .decode.arg(expl_vars)
  if (is.null(dep_var)) {
    stop('Unsupervised version is not implemented yet')
  }
  # If no expl_vars are given, take them all except dep_var
  if (is.null(expl_vars)) {
    tryCatch({
      by.col <- get('by.col', envir = .mycache)
      expl_vars = setdiff(colnames(df), c(by.col, dep_var))
    }, error = function(e){stop('Run init first')})
  }

  ntree = max(min(10, nrow(df)), 150)
  forest = randomForest::randomForest(df[,expl_vars], y = df[,dep_var],
                        nodesize = nodesize,
                        norm.votes = FALSE,
                        ntree = ntree,
                        ...)

  # Here we need to return a lighter object, that allows classification but not
  # patient identification.
  forest = .deidentify(forest)
  return(forest)
}

#'
#' @description: De-identify a randomForest object, and at the same time
#'   make the ouptut lighter to send to the client.
#'
.deidentify <- function(forestObject) {
  # No need what concerns the training set
  forestObject$predicted = NULL
  forestObject$votes = NULL
  forestObject$oob.times = NULL
  forestObject$proximity = NULL
  forestObject$localImportance = NULL
  forestObject$inbag = NULL
  forestObject
}

#predict.forest.DS2 <- function(newData, dep_var=NULL, expl_vars=NULL, ...) {
#}

