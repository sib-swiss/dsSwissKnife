asDSS <- function(desttype, obj){

  # first just remove an eventual leading dot:
  desttype <- sub('^\\.','',desttype, perl=TRUE)
  # take only the first word (dots are allowed)
  # probably a bit overkill as I am  pasting an 'as.' before the code and not eval-ing :
  desttype <- strsplit(desttype, '[^\\w,\\.]', perl = TRUE)[[1]][1]
  my_call <- paste0('as.', desttype)
  obj <- .decode.arg(obj)
  do.call(my_call, unname(.extract(obj)))
}
