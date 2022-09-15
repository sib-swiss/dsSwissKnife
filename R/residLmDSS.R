residLmDSS <- function(outcomes, indvars, data){
  outcomes <- .decode.arg(outcomes)
  indvars <- .decode.arg(indvars)

   if(is.list(outcomes)){ # it's a list of vectors of ordered model coefficients - one per dep var
    depvars <- names(outcomes)
    data <- data[, unique(c(depvars, indvars))]
    out <- sapply(depvars, function(depvar){
      #deal with missing features:
    #  missingCols <- setdiff(indvars, colnames(data))
     # data[,missingCols] <- sapply(missingCols, function(x) rnorm(nrow(data))) # slap some normal distributions in

      predictors <- data[, setdiff(indvars, depvar)] # indvars can contain depvar
      predictors <- data.frame(Intercept = 1, predictors) # add the intercept
      actual <- data[, depvar]
      coefs <- outcomes[[depvar]]
      predictions <- rowSums(t(t(predictors) * coefs))
      res <- actual - predictions

      res
    }, simplify = FALSE)
  } else {  # it's a vector of dep vars - we use a local lm
    data <- data[, unique(c(outcomes, indvars))]
    out <- sapply(outcomes, function(x){
      localIndVars <- setdiff(indvars, x)
      if(length(localIndVars) == 0){
        return(rep(NA, nrow(data)))  # don't spoil it for everyone else
      }
      formula <- paste0(x, ' ~ ', paste(localIndVars, collapse = ' + '))
      mdl <- lm(formula, data = data)
      return(resid(mdl))
    }, simplify = FALSE)

  }

  as.data.frame(out)

}






