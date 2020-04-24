#' @title Try to find an appropriate formula to pass to reshape2::dcast
#' @description The CDISC format can be cumbersome for analysis. The solution would be to transform the data to wide format
#' using the function dcast from the package reshape2. However, as the actual data is not visible, choosing the right
#' widening formula is challenging. This function applies some simple heuristics on the provided parameters and tries to guess
#' a possible formula. This will not be necessarily the best option (and often it will fail completely) - it can still provide
#' a starting point for further refining.
#' @return a formula (char) that can be (modified and) passed further to the function 'widen'
#' @param what, a data.frame
#' @param cols, a vector of columns used in the formula. They must exist  in 'what' (the previous argument). If null, all the columns from 'what' will be used.
#' @param by.col typically the patient id, the column that will end up as key of the resulting 'wide' data.frame. In most cases this
#' can be left null as it's been set when the package was loaded
#'
#' @export
#'
#'

suggestPivotFormula <- function(what, cols = NULL ,  by.col = NULL){

  by.col <- .decode.arg(by.col)
  cols <- .decode.arg(cols)
  if (is.null(by.col)){
    tryCatch(by.col <- get('by.col', envir = .mycache), error = function(e){stop("by.col is not specified and there's no default value")})
  }


  #if we don't have cols, guess them from 'what'
  if(is.null(cols)){
    cols <- colnames(what)
  }
  # we group by by.col so we don't need it in the column list:
  cols <- cols[!(cols == by.col)]

  # dplyr magic; get the total counts and the counts by by.col
  grp <- dplyr::group_by_(what, by.col)
  distinct.counts <-as.data.frame(dplyr::summarise_all(what[, cols], .n_distinct.no.na))
  grouped.counts <- as.data.frame(Map(max, dplyr::summarise_all(grp[,c(by.col,cols)],.n_distinct.no.na)[cols]))

  #which counts are equal? they are candidates for the right hand of the formula:
  rh <-colnames(grouped.counts)[grouped.counts == distinct.counts & grouped.counts > 1]
  if(length(rh) == 0){
    stop("There's nothing I can find for the right hand of the formula.")
  }
  # which grouped counts are 1? they appear once per by.col, so left hand of the formula (like by.col)
  lh <-colnames(grouped.counts)[grouped.counts == 1]
  lh <- gsub(', ',' + ', toString(unique(c(lh, by.col)))) # add by.col in the left hand only if it's not there already
  # build the formula:
  rh <- gsub(', ', ' + ', toString(rh))
  retval <- paste0(lh, ' ~ ', rh)
  retval
}


#' @title Create a wide data.frame using reshape2::dcast
#' @description  This function  transforms the data from long (ex: CDISC) to wide format
#' using the function dcast from the package reshape2.
#' @param what a  data.frame
#' @param cols a vector of columns used in the formula. They must exist in 'what'. If null, all the columns from 'what' will be used.
#' @param measure name of the column which stores values, this goes straight into the parameter value.var in reshape2::dcast
#' @param formula the formula to use by reshape2::dcast. If left null an attempt will be made to guess it with suggest.pivot.formula.
#' @param by.col typically the patient id, the column that will end up as key of the resulting 'wide' data.frame. In most cases this
#' can be left null as it's been set when the package was loaded
#' @param completeCases a logical, keep only complete cases?
#' @return a data.frame in wide format
#' @export
#'

widenDSS <- function(what, measure, cols = NULL, formula = NULL,  by.col = NULL, completeCases = FALSE , fun.aggregate = NULL){
  by.col <- .decode.arg(by.col)
  if(is.null(by.col)){
    tryCatch(by.col <- get('by.col', envir = .mycache), error = function(e){stop('Run init first')})
  }
  # add the df name into column names to avoid confusion later
  # also make "what" a dataframe if necessary
  if(!is.data.frame(what)){
    df.name <- what
    what <- get(what, envir = parent.frame())
  } else {
    df.name <- as.list(match.call())[['what']]
  }
  #if we are not given colnames, use all of them
  cols <- .decode.arg(cols)
  if(is.null(cols)){
    cols <- colnames(what)
  }
  have.formula <- TRUE # to decide later if we show it in the error message
  formula <- .decode.arg(formula)
  if(is.null(formula)){
    have.formula <- FALSE
    formula <- suggestPivotFormula(what, cols, by.col)
  }
  if(is.na(formula)){
    #nothing I can do
    return(what)
  }
  # build the pivot columns from the formula:
  pivot.cols <- strsplit(formula, '~')[[1]][[2]]
  pivot.cols <- strsplit(pivot.cols, '+', fixed = TRUE)
  pivot.cols <- unlist(Map(function(x) gsub('\\s+', '', x, perl = TRUE), pivot.cols))

  Map(function(x) {
    what[,x] <<- unlist(Map(function(y) paste0(x, '.' ,y), what[,x]))
  }, pivot.cols )
  # !!!!it appears that we can have NAs in value.var, so get rid of them now:
  what <- what[!is.na(what[[measure]]),]
  # deal with fun.aggregate
  fun.aggregate <- .decode.arg(fun.aggregate)
  if(!is.null(fun.aggregate)){

    # an evil eval follows, we have to lock all objects and envs before, unlock them after + get rid of any new objects created in the eval if any

    safe.objs <- .ls.all()
    # lock everything so no objects can be changed
    .lock.unlock(safe.objs, lockBinding)
    tryCatch({
      fun.aggregate <- eval(parse(text = fun.aggregate))
    },
    error = function(e){
      # if anything happens, unlock and cleanup:
      .lock.unlock(safe.objs, unlockBinding)
      .cleanup(safe.objs)
      stop(e)
    })
    # unlock back everything
    .lock.unlock(safe.objs, unlockBinding)
    # get rid of any sneaky objects that might have been created in the eval as side effects
    .cleanup(safe.objs)
  }
  msg <- capture.output(wide <- reshape2::dcast(what, formula(formula), value.var = measure, fun.aggregate = fun.aggregate), type=c('message')) # when the widening doesn't work it gives us a message, capture it
  if (length(msg) > 0  && grepl('Aggregation function missing', msg, ignore.case = TRUE)){
    msg <- 'The widening would result in meaningless counting of the values. Please either change the formula or provide an aggregation function (mean for instance).'
    if(!have.formula) msg <- paste0(msg, ' The formula was ', formula)
    stop(msg, call. = FALSE)
  }
  #if we didn't end up with unique by.col better say something:
  if(length(wide[, by.col]) !=  length(unique(wide[, by.col]))){
    warning(paste0(by.col, ' is not unique.'), call. = FALSE)
  }
  cl <- colnames(wide)

  # fiddling with weird chars:
  Encoding(cl) <- 'latin1'
  cl <- gsub('Ã', '' ,cl)
  cl <- gsub('%', 'percent' ,cl, fixed = TRUE)


  colnames(wide) <- make.names(cl)

  # count the NAs in the original df and in the wide one, if there are more in the wide version, again we'll probably
  # need to tweak the formula
  nas <- sapply(list('wide' = wide, 'what' = what), function(y){
    sum(sapply(colnames(y), function(x){
      sum(is.na(y[,x]))
    }))
  })

  if(completeCases){
    wide <- wide[complete.cases(wide),]
  }
  as.data.frame(lapply(wide, function(x){
    if (is.character(x)){
      return(as.factor(x))
    }
    x
  }))
}

.n_distinct.no.na <- function(...){
  # nothing to see here, simplifying a bit the ...counts function calls in suggestivot.formula
  dplyr::n_distinct(..., na.rm = TRUE)
}
