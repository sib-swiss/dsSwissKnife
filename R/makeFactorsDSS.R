makeFactorsDSS <- function(df){
  df <- get(df, envir = parent.frame())
  as.data.frame(sapply(df, function(y){
    if(length(intersect(class(y) , c('character', 'Date', 'POSIXct', 'POSIXlt', 'POSIXt'))) >0 ){
      return(factor(y))
    } else {
      return(y)
    }
  },simplify = FALSE))
}
