
.init <- function(){
  .mycache <<- new.env(parent=.GlobalEnv)
  #hardcode for now the name of the hidden column(s):
  hidden <- getOption('hidden.fields')
  assign('hidden', getOption('hidden.fields'), envir = .mycache)
  #allowed.funcs <- c('abs', 'round', 'ceiling', 'floor', 'signif', 'length')
  #ign('allowed.funcs', allowed.funcs, envir = .mycache)
  assign('allowed.funcs', getOption('allowed.functions'), envir = .mycache)
}



.split.numerics <- function(x, collist = NULL){
  nums <- sapply(x, is.numeric)
  nums <- nums[is.null(collist) | names(nums) %in% collist] # only collist if supplied
  nums <- nums[nums == TRUE]
  #if(length(nums) == 0){
  #  return(NA)
  #}
  #list(numerics = as.data.frame(subset(x, TRUE, nums)), others = as.data.frame(subset(x, TRUE, !nums)))
  hidden <-  get('hidden', envir = .mycache)
  nums <- nums[!(names(nums) %in% hidden)] # no SUBJID
  list(numerics = x[,names(nums), drop= FALSE], others = x[,!(colnames(x) %in% names(nums)), drop = FALSE])
}

#' @title Decode from base64 and deserialize from json if necessary
#' @description Work around the restrictions imposed by the Opal server on function arguments
#' The Opal server is very picky as regards function arguments. The workaround is
#' to serialize and encode them on the client and strip the right padding.
#' @details It looks for the string 'base64' in the argument to determine if it's encoded
#' @param some.thing the thing to be decoded and deserialized from json if necessary
#' @return the decoded and deserialized argument
#'
.decode.arg <- function(some.thing, simplifyMatrix = FALSE){

  if(length(some.thing) == 1 && grepl('base64', some.thing, ignore.case = TRUE)){
    some.thing <- gsub('base64', '', some.thing, ignore.case =TRUE)
    my.dictionary = c('-plus-' = '+', '-slash-' = '/', '-equals-' = '=')
    sapply(names(my.dictionary), function(x){
      some.thing <<- gsub(x, my.dictionary[x], some.thing)
    })
    some.thing <- jsonlite::fromJSON(RCurl::base64Decode(some.thing), simplifyMatrix = simplifyMatrix)
  }
  return(some.thing)
}
