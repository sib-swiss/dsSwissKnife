partMean <- function(x, na.rm = TRUE){

  # must be datashield valid (more than <datashield.privacyLevel> rows):
  if(!dsBase::isValidDS(x)){
    return(NA)
  }

  return(list(mean = mean(x, na.rm = na.rm), len = length(x)))

}
