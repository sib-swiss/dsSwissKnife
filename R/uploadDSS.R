uploadDSS <- function(name, payload, is.first =  TRUE, is.last = TRUE, spec.obj = NULL){
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
    if(spec.obj == 'uwot_model'){
      fname <- tempfile()
      writeBin(get(name, envir = myenv), fname)
      assign(name, uwot::load_uwot(fname), envir = myenv)
      file.remove(fname)
    }

  } else {
    assign(name, actual, envir = .mycache)
  }

  return(TRUE)
}
