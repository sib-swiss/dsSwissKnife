#'
#' @title execute data() in the server session
#' @description  load specified dataset
#' @param x a character, name of a data frame
#' @param start, keep rows starting with this
#' @param end, keep rows until here
#' @param package a character, name of the package where to look for datasets
#' @return  a numeric vector which contains the minimum and the maximum

#' @export
partialData <- function(what, start = NULL, end  = NULL, package = NULL){

  data(list = what, package = package, envir = environment())

  my.df <- get(what)
  if (is.null(start)){
    start <- 1
  }
  if(is.null(end)){
    end <- nrow(my.df)
  }
  my.df <- my.df[start:end,]
  assign(what, my.df, pos = parent.frame())
  TRUE
}
