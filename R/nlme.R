#' @export
groupedDataDSS <- function(arglist){
  arglist <- .decode.arg(arglist)
  arglist$formula <- as.formula(arglist[['formula']])
  arglist$data <- get(arglist[['data']], envir = parent.frame())
  do.call(nlme::groupedData, arglist)
}

#' @export
nlme_lmeDSS <- function(arglist){
  arglist <- .decode.arg(arglist)
  arglist$fixed <- as.formula(arglist[['fixed']])
  arglist$random <- as.formula(arglist[['random']])
  arglist$data <- get(arglist[['data']], envir = parent.frame())
  ret <- do.call(nlme::lme, arglist)
  ret[['data']] <- NULL
  ret[['call']] <- NULL
  ret[['residuals']] <- NULL
  ret[['groups']] <- NULL
  ret

}
