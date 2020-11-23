makeFactorsDSS <- function(df){
  as.data.frame(sapply(df, function(y){
    if(length(intersect(class(y) , c('character', 'Date', 'POSIXct', 'POSIXlt', 'POSIXt'))) >0 ){
      return(factor(y))
    } else {
      return(y)
    }
  },simplify = FALSE))
}
