#' @export
cutDSS <- function(arglist){

  arglist <- .decode.arg(arglist)

  if(arglist[['in.place']]){
    new.name <- arglist$x
    arglist$in.place <- NULL
  } else {
    new.name <- arglist$new.name
  }

  arglist$new.name <- NULL
  if(!is.null(arglist$df)){
    retname <- arglist$df
    mydf <- get(retname, envir = parent.frame())
    arglist$df <- NULL
    arglist$x <- mydf[,arglist$x]
    mydf[, new.name] <- do.call(cut, arglist)
    assign(retname, mydf, envir = parent.frame())
  } else {
    retname <- new.name
    arglist$x <- get(arglist$x, envir = parent.frame())
    assign(retname, do.call(cut, arglist), envir = parent.frame())
  }
  return(retname)
}
