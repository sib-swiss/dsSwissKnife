#' @export
selfUpgrade <- function(other.package = NULL ,method = NULL, lib = NULL, verbose = FALSE ){
  # systemd sometimes goes and deletes the rserv tempdir, we need to recreate it if necessary
  tmp <- tempdir()
  if(!dir.exists(tmp)){
    dir.create(tmp)
  }
  if(!is.null(lib)){
    lib = .decode.arg(lib)
  }
  x <- list( dsSwissKnife = capture.output(install.packages('dsSwissKnife', lib = lib, repos=c('https://rhap-fdb01.vital-it.ch/repo', 'http://rhap-fdb01.vital-it.ch/repo', 'https://stat.ethz.ch/CRAN', 'http://stat.ethz.ch/CRAN'),
                                                       method = method), type = c('message')))
  if(!is.null(other.package)){
    x[[other.package]] <- capture.output(install.packages(other.package, lib = lib,
                                                          repos=c('https://rhap-fdb01.vital-it.ch/repo', 'http://rhap-fdb01.vital-it.ch/repo', 'https://stat.ethz.ch/CRAN', 'http://stat.ethz.ch/CRAN'), method = method),type = c('message'))
    if(other.package == 'BiocManager'){
      BiocManager::install(ask = FALSE)
    }
  }
  if(verbose){
    x[['installed']] <- installed.packages()
    x[['libs']] <- .libPaths()
    x[['mem']] <- system('free -m', intern = TRUE)
  }
  return(x)

}


biocInstall <- function(...){
  x <- list(...)
  x[['ask']] <- FALSE
  capture.output(do.call(BiocManager::install, x), type= c('output', 'message'))
}
