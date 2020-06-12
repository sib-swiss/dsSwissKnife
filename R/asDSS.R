asDSS <- function(desttype, objname){
  # guard against code injections:
  # first just remove an eventual leading dot:
  desttype <- sub('^\\.','',desttype, perl=TRUE)
  # take only the first word:
  desttype <- strsplit(desttype, '[^\\w,\\.]', perl = TRUE)[[1]][1]

  x <- get(objname, envir = parent.frame())
  my_code <- paste0('as.',desttype,'(x)')
  eval(parse(text = my_code))

}
