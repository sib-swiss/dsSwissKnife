#' @export
clogitDSS <- function(arglist){
  # clogit is weird, so the following contorsions are necessary:
  coxph <- survival::coxph
  Surv <- survival::Surv

  arglist <- .decode.arg(arglist)
  arglist$formula <- as.formula(arglist[['formula']])
  arglist$data <- get(arglist[['data']], envir = .GlobalEnv)
  arglist$data <- arglist$data[, .trim_hidden_fields(names(arglist$data))]
  arglist$x <- FALSE
  arglist$model <- FALSE

  ret <- do.call(survival::clogit, arglist)
  # stuff is disclosed in the call so get rid of it:
  ret[['userCall']] <- NULL
  ret[['call']] <- NULL
  # residuals too:
  ret[['residuals']] <- NULL
  ret
}
