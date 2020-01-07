.onLoad <- function(...){
  if(is.null(getOption('hidden.fields'))){
    options(hidden.fields = c('SUBJID', 'USUBJID'))
  }
  if(is.null(getOption('allowed.functions'))){
    options(allowed.functions = c('abs', 'round', 'ceiling', 'floor', 'signif', 'length'))
  }
  .init()
  options(stringsAsFactors=TRUE)

}
