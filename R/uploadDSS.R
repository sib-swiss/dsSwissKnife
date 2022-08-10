uploadDSS <- function(name, payload, is.first =  TRUE, is.last = TRUE){
  # get an encoded string and decode it as an object in the session
  # the string can come in chunks, if so concatenate the chunks in .mycache and at the last one decode in the session
  myenv <- parent.frame()
  if(is.first){
    prev <- ''
  } else {
    prev <- get(name, envir = .mycache)
    rm(list = name, envir = .mycache)
  }
  actual <- paste0(prev, payload)
  if(is.last){
    assign(name, .decode.arg(actual), envir = myenv)
  } else {
    assign(name, actual, envir = .mycache)
  }
  return(TRUE)
}
