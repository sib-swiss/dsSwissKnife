#' @export
joinDSS <- function(what, type='full', bycol = NULL){

  bycol <- .decode.arg(bycol)
  if(is.null(bycol)){
    tryCatch(bycol <- get('by.col', envir = .mycache), error = function(e){stop('Run init first')})
  } else {
    bycol <- unlist(bycol)
  }
  where <- parent.frame()
  what <- lapply(.decode.arg(what), function(x) {
                                      tryCatch(get(x, envir = where), error = function(e){
                                        if(grepl('$',x)){
                                          temp <- unlist(strsplit(x, "\\$"))
                                          lst <- get(temp[1], envir = where)
                                          lst[[temp[2]]]
                                        }
                                      })
                                    })

  if (type == 'full'){
    ret <- Reduce(function(x,y) dplyr::full_join(x,y, by = bycol), what )
  }
  if (type == 'inner'){
    ret <- Reduce(function(x,y) dplyr::inner_join(x,y, by = bycol), what )
  }
  if (type == 'left'){
    ret <- Reduce(function(x,y) dplyr::left_join(x,y, by = bycol), what)
  }
  if (type == 'right'){
    ret <- Reduce(function(x,y) dplyr::right_join(x,y, by = bycol), what )
  }
  if (type == 'semi'){
    ret <- Reduce(function(x,y) dplyr::semi_join(x,y, by = bycol), what )
  }
  if (type == 'anti'){
    ret <- Reduce(function(x,y) dplyr::anti_join(x,y, by = bycol), what)
  }
  ret
}
