tableDSS <- function(x){
  myEnv <- parent.frame()
  x <- .betterExtract(.decode.arg(x), myEnv)
  out <- table(x)
  out[out <= getOption('datashield.privacyLevel', 5)] <- 0
  out
}
