tableDSS <- function(x){
  out <- table(x)
  out[out <= getOption(datashield.privacyLevel, 5)] <- 0
  out
}
