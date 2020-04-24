

#' @export
addDaysInterval <-function(df, arglist){
  arglist <-.decode.arg(arglist)
  Map(function(x){
    df[,x] <<- .elapsedDays(df[,arglist[[x]]$end_date], df[,arglist[[x]]$start_date])
  }, names(arglist))
  df
}

.elapsedDays <- function(end_date, start_date) {

  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  as.numeric(difftime(as.Date(ed), as.Date(sd), units = "days"))
}
