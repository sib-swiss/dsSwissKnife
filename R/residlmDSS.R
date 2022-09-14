resLmDSS <- function(models, indvars, data){
  models <- .decode.arg(models)
  indvars <- .decode.arg(indvars)
  depvars <- names(models)


 data <- data[, unique(c(depvars, indvars))]

  if(1==1){ # it's a list of vectors of ordered model coefficients
    sapply(depvars, function(depvar){
      #deal with missing features:
      missingCols <- setdiff(indvars, colnames(data))
      data[,missingCols] <- sapply(missingCols, function(x) rnorm(nrow(data))) # slap some normal distributions in

      predictors <- data[, setdiff(indvars, depvar)] # indvars can contain depvar
      predictors <- data.frame(Intercept = 1, predictors) # add the intercept
      actual <- data[, depvar]
      coefs <- models[[depvar]]
      predictions <- rowSums(t(t(predictors) * coefs))
      res <- actual - predictions

      scale(res)
    })

  }



}

myLmResid <- function(depvar, coefs, indvars, data){

  #deal with missing features:
  missingCols <- setdiff(indvars, colnames(data))
  data[,missingCols] <- sapply(missingCols, function(x) rnorm(nrow(data))) # slap some normal distributions in

  predictors <- data[, setdiff(indvars, depvar)] # indvars can contain depvar
  predictors <- data.frame(Intercept = 1, predictors) # add the intercept
  actual <- data[, depvar]
  predictions <- rowSums(t(t(predictors) * coefs))
  res <- actual - predictions

  scale(res)
}




