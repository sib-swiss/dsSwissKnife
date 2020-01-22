# sum of squared distances (for a saner calculation of global variance):
partSsd <- function (xvect, xpoint ) {
  xect <- xvect[!is.na(xvect)]
  # if no xpoint we use the local mean
  if(missing(xpoint)){
    xpoint <- mean(xvect, na.rm = TRUE)
  }
  check <- dsBase::isValidDS(xvect)
  if (check) {
    res <- sum((xvect - xpoint)^2)
    len <- length(xvect)
  }
  else {
    res <- NA
    len <- NA
  }
  return(list(ssd = res, len = len ))
}