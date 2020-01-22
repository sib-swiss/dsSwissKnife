#' @export
colnamesDS <- function(df, to.replace = NULL, cols = NULL ){
  cols <- .decode.arg(cols)
  to.replace <- .decode.arg(to.replace)
  df_name <- deparse(substitute(df))
  if (!is.null(cols)){
    if(!is.null(to.replace)){
      temp <- colnames(df)
      temp[temp %in% to.replace] <- cols
      cols <- temp
    }
    colnames(df) <- cols
    assign(df_name, df, envir = parent.frame())
  }
  return(colnames(df))

}
