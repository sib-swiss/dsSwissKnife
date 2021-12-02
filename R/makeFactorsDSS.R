makeFactorsDSS <- function(df){
  hidden <- get('hidden', envir = .mycache)
  as.data.frame(sapply(.trim_hidden_fields(colnames(df)), function(y){
    if(length(intersect(class(df[,y]) , c('character', 'Date', 'POSIXct', 'POSIXlt', 'POSIXt'))) >0 ){
      return(factor(df[,y]))
    } else {
      return(df[,y])
    }
  },simplify = FALSE), stringsAsFactors = FALSE)
}
