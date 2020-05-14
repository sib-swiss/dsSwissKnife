#' @title  Initialize the environment
#' @description  Set a number of defaults for some of the functions.
#' These are:
#'
#' 1) hidden.fields - fields that must show no information (patient identifiers for instance)
#'
#' 2) allowed.functions - supplementary functions allowed in dssDeriveColumn (see the documetation for that function)
#'
#' 3) join.pivot.col - the default column to join (or pivot) by. In the absence of this default
#' the 'by.col' argument in dssJoin and dssPivot becomes mandatory
#'
#' In order to set/modify any of these options the node administrator must create an .Rprofile file
#' in the rserver conf directory (normally /var/lib/rserver/conf) and populate it with the necessary
#' R commands to set the above options (see example). Then the foloowing line must be added to the file
#' /var/lib/rserver/conf/Rserv.conf:
#' source /var/lib/rserver/conf/.Rprofile
#
#'@examples:
#' # content of the /var/lib/rserver/conf/.Rprofile file that adds as.POSIXct among
#' # the allowed functions in dssDeriveColumn:
#' options(allowed.functions='as.POSIXct')
#'
#'
.init <- function(){
  .mycache <<- new.env(parent=.GlobalEnv)
  assign('hidden', getOption('hidden.fields'), envir = .mycache)
  allowed.funcs <- c('abs', 'round', 'ceiling', 'floor', 'signif', 'length', 'as.Date', 'as.character', 'as.numeric', getOption('allowed.functions'))
  #ign('allowed.funcs', allowed.funcs, envir = .mycache)
  assign('allowed.funcs', allowed.funcs, envir = .mycache)
  if(!is.null(getOption('join.pivot.col'))){
    assign('by.col', getOption('join.pivot.col'), envir = .mycache)
  }
  options(stringsAsFactors = TRUE)
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


.extract <- function (input, start.env = NULL){
  #modified version of dsBase:::extract
  #it returns a list of (inputname = object)
  #works for dataframes embedded in lists

  input <- unlist(input)

  output <- list()
  for (i in input) {

    inputterms <- unlist(strsplit(i, "\\$", perl = TRUE))
    if(!is.null(start.env)){
      inputterms <- inputterms[2:length(inputterms)]
      inputterms <- inputterms[!is.na(inputterms)]
      obj <- as.environment(start.env)
    } else {
      obj <- parent.frame()

    }
    for(objname in inputterms){
      this.env <- as.environment(obj)


      obj <- get(objname, envir = this.env)
    }
    output[[i]] <- obj
  }

  return(output)
}


.dsBase_extract <- function (input){
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)) {
    inputterms <- unlist(strsplit(input[i], "\\$", perl = TRUE))
    if (length(inputterms) > 1) {
      obj1 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][2]
    }
    else {
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl = TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list(holders = output1, elements = output2)
  return(output)
}

#'
#' @title returns all objects in all environments
#' @description helper function for dssSubset and dssPivot
#' @param start a character the environment name where to start (default .GlobalEnv)
#' @return  a list of environment names and the respective objects defined in each environment
#'

.ls.all <- function(start = '.GlobalEnv'){
  envir <- get(start)
  objs <- ls(envir, all.names = TRUE)
  ret <- list()
  ret[[start]] <- objs
  more.envs <- names(which(sapply(objs, function(x)is.environment(get(x)))==TRUE))
  c(ret,sapply(more.envs,function(x) ls(get(x), all.names = TRUE), USE.NAMES = TRUE, simplify = FALSE))

}

#'
#' @title locks or unlocks bindings in environments
#' @description helper function for dssSubset and dssPivot
#' @param what a list of  environments and their respective objects - the output of ls.all above
#' @param func a function, either lockBinding or unlockBinding
#'

.lock.unlock <- function(what , func){
  stopifnot(deparse(substitute(func)) %in% c('lockBinding', 'unlockBinding'))
  invisible(lapply(names(what), function(x){
    lapply(what[[x]], function(y){
      func(y,get(x))
    })
  }))

}

#' @title removes objects from the current workspace
#' @description helper function for dssSubset and dssPivot
#' @param what a list of  environments and their respective objects - the output of a previous call to ls.all
#' @param start a character the environment name where to start (default .GlobalEnv)
#'


.cleanup <- function(initial, start = '.GlobalEnv'){
  objs <- .ls.all(start)
  new.envs <- setdiff(names(objs), names(initial))
  Map(function(x){
    rm(get(x))
    objs[x] <- NULL
  }, new.envs)
  invisible(Map(function(x){
    new.objs <- setdiff(objs[[x]], initial[[x]])
    rm(list = new.objs, pos = get(x))
  }, names(objs)))

}


.dsBase_isValidDSS <- function (obj) {
  nfilter <- .dsBase_setFilterDSS()
  if (class(obj) == "character" | class(obj) == "integer" |
      class(obj) == "logical" | class(obj) == "numeric") {
    if (length(obj) > 0 & length(obj) < nfilter) {
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  }
  else {
    if (class(obj) == "factor") {
      tt <- tabulate(obj)
      xx <- which(tt > 0 & tt < nfilter)
      if (length(xx) > 0) {
        return(FALSE)
      }
      else {
        return(TRUE)
      }
    }
    else {
      if (class(obj) == "data.frame" | class(obj) == "matrix") {
        if (dim(obj)[1] > 0 & dim(obj)[1] < nfilter) {
          return(FALSE)
        }
        else {
          return(TRUE)
        }
      }
      else {
        return(FALSE)
      }
    }
  }
}

.dsBase_setFilterDSS <- function (x = getOption("datashield.privacyLevel", default = 5)) {
  # from dsBase
  a <- as.numeric(as.character(x))
  return(a)
}

.dsBase_numNADSS <- function (xvect){
  # from dsBase
  out <- length(which(is.na(xvect)))
  return(out)
}

