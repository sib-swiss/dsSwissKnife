
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

#forestDSS <- function(train = NULL, test = NULL) {
forestDSS <- function(...) {
  arglist <- list(...)

  if (arglist[[1]] == 'test'){
   # test <- .decode.arg(test, simplifyMatrix = TRUE)
   # test$testData <- get(test$testData, envir = parent.frame())
    testData <- get(arglist[[2]], envir = parent.frame())
    #arglist <- within(arglist, rm(testData))
    arglist <- arglist[-c(1,2)]
    enc <- Reduce(paste0, arglist)

    forest <- .decode.arg(enc)
#    test$forest <- sapply(test$forest, function(x){
  #    if(exists('err.rate', where=x)){
        # redo the matrix if necessary
  #      x$err.rate <- Reduce(rbind, x$err.rate)
  #      dimnames(x$err.rate) <- list(NULL, 'OOB')
  #    }
      # reclass it
#      class(x) <- c('randomForest')
#
#    }, simplify = FALSE)
 #   return(do.call(.predict, test))
    prediction <- .predict(forest, testData)

    confmat <- table(prediction, testData[,forest[[1]]$dep_var])
     if(length(rownames(confmat)) <= length(colnames(confmat))){ # can't use diag(), sometimes it's not square
        dg <- unlist(lapply(rownames(confmat), function(x) confmat[x,x]))
      } else {
        dg <- unlist(lapply(colnames(confmat), function(x) confmat[x,x]))
      }


    acc <- sum(dg)/sum(confmat)
    return(list(prediction = prediction, confusion_matrix = confmat, accuracy = acc))
  } else if(arglist[[1]] == 'train'){  # done with the test part
    train <- .decode.arg(arglist[[2]])
    x <- get(train$what, envir = parent.frame())
    # If no expl_vars are given, take them all except dep_var
    if (is.null(train$expl_vars)){
      try({
        by.col <- get('by.col', envir = .mycache)
        train$expl_vars = setdiff(colnames(x), c(by.col, train$dep_var))
      }, silent = TRUE)

    }
    train$what <- NULL
    train <- train[!sapply(train, is.null)]
    train$x <- x[,train$expl_vars]
    train$y <- x[,train$dep_var]
    if(!exists('ntree', where = train)){
      train$ntree <- max(min(10, nrow(df)), 150)
    }
    if(!exists('nodesize', where = train)){
      train$nodesize <- 5
    }


    forest <- do.call(randomForest::randomForest, train)
  } else {
    stop('First argument must be one of "train" or "test".', call. = FALSE)
  }
  # Here we need to return a lighter object, that allows classification but not
  # patient identification.
  forest = .deidentify(forest)
  forest$dep_var <- train$dep_var
  return(forest)
}




#' @title: Remove potentially disclosive information
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
  forestObject$call <- NULL
  if(exists('err.rate', where=forestObject)){
     forestObject$err.rate <- forestObject$err.rate[,'OOB',drop = FALSE]
  }
  forestObject$y <- NULL
  forestObject
}

#predict.forest.DS2 <- function(newData, dep_var=NULL, expl_vars=NULL, ...) {
#}


#'
#' @title Predict classification of new patients
#'   using the forests of our different data sources.
#' @description For classification, we sum the votes from all the forests,
#    and return the class with the most votes overall.
#    For regression, we average the estimated values from all the forests.
#' @param forests: a list of randomForest objects.
#' @param testData: [data frame] new data to classify using the forests.
#' It must have at least the columns in `expl_vars`.
#' (We want to predict the value of `dep_var` for it.)
#' @return a vector of length `nrow(testData)`.
#'
.predict <- function(forests, testData) {

  nforests = length(forests)
  predictionType = forests[[1]]$type

  # For classification, we sum the votes from all the forests,
  # and return the class with the most votes overall.
  if (predictionType == "classification") {
    # Forests may end up with different classes. Take the union of them all here.
    classes = Reduce(function(cls, forest) union(cls, forest$classes), forests, NULL)
    # Init value for the Reduce
    p0 = matrix(0, nrow(testData), length(classes))
    colnames(p0) = classes
    # Sum the votes
    sumVotes = Reduce(function(p1, forest) {
      p2 = p0
      p2[, forest$classes] = predict(forest, testData, type="vote") * forest$ntree
      # The result is a matrix with nrow(testData) rows and length(forest$classes) columns.
      # It should contain vote counts (ints), but there is a bug in this lib and it returns the
      #  fraction of votes, so we need to multiply by the number of trees.
      #  Forests may have different number of trees, depending on dataset size.
      return(p1 + p2)
    }, forests, p0)

    prediction = apply(sumVotes, 1, function(row) {
      maxclass = classes[which(row == max(row))]
      # if there's more than one, toss the coin:
      if(length(maxclass) >1 ){
        maxclass  <- sample(maxclass,1)
      }
      return(maxclass)
    })

    # For regression, we average the estimated values from all the forests.
  } else if (predictionType == "regression") {
    p0 = rep(0, nrow(testData))
    sumEstimations = Reduce(function(p1, forest) {
      p2 = predict(forest, testData, type="response")
      # The result is a vector with nrow(testData) elements.
      return(p1 + p2)
    }, forests, p0)
    prediction = sumEstimations / nforests
  }

  return(prediction)
}

