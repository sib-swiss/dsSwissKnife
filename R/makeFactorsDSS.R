makeFactorsDSS <- function(df){
  hidden <- get('hidden', envir = .mycache)
  as.data.frame(sapply(colnames(df), function(y){
    if(length(intersect(class(df[,y]) , c('character', 'Date', 'POSIXct', 'POSIXlt', 'POSIXt'))) >0  && !(y %in% hidden)){
      return(factor(df[,y]))
    } else {
      return(df[,y])
    }
  },simplify = FALSE), stringsAsFactors = FALSE)
}
