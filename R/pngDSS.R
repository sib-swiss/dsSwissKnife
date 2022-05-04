pngDSS <- function(...){
  fname <- paste0(tempdir(check = TRUE), '/canvas.png')
   png(filename = fname,...)
}
