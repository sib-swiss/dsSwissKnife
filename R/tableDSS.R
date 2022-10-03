tableDSS <- function(x){
  x <- .betterExtract(.decode.arg(x))
  out <- table(x)
  out[out <= getOption(datashield.privacyLevel, 5)] <- 0
  out
}
