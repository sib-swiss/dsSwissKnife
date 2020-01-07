
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
