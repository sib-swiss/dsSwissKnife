#' @export
selfUpgrade <- function(other.package = NULL ,method = NULL, lib = NULL, extra = NULL, verbose = FALSE ){
  # extra is for arguments passed to wget or curl
  # to ignore cert problems: method = 'wget', extra = '--no-check-certificate' or method = 'curl', extra = '-k'
  # systemd sometimes goes and deletes the rserv tempdir, we need to recreate it if necessary
  tmp <- tempdir()
  if(!dir.exists(tmp)){
    dir.create(tmp)
  }
  if(!is.null(lib)){
    lib = .decode.arg(lib)
  }
  x <- list( dsSwissKnife = capture.output(install.packages('dsSwissKnife', lib = lib,
                                                            repos=c("https://sophia-fdb.vital-it.ch/SIB-R", 'https://sophia-fdb.vital-it.ch/CRAN', "https://sophia-fdb.vital-it.ch:8443/SIB-R", 'https://sophia-fdb.vital-it.ch:8443/CRAN'),
                                                            method = method), type = c('message')))
  if(!is.null(other.package)){
    x[[other.package]] <- capture.output(install.packages(other.package, lib = lib,
                                                          repos=c('https://sophia-fdb.vital-it.ch/SIB-R',  'https://sophia-fdb.vital-it.ch/CRAN', "https://sophia-fdb.vital-it.ch:8443/SIB-R", 'https://sophia-fdb.vital-it.ch:8443/CRAN'),
                                                          method = method),type = c('message'))
    if('BiocManager' %in% other.package){
      BiocManager::install(ask = FALSE)
    }
    if('tensorflow' %in% other.package){
      reticulate::install_miniconda()
      tensorflow::install_tensorflow(method='conda', version='cpu')
    }
  }
  if(verbose){
    x[['installed']] <- installed.packages()
    x[['libs']] <- .libPaths()
    x[['mem']] <-tryCatch(system('free -m', intern = TRUE),
                          error = function(e){
                               tryCatch(system('cat /proc/meminfo  | grep MemFree', intern = TRUE),
                                        error = function(f) paste0(e, ' - ', f) )
                        })
  }
  return(x)
}


biocInstall <- function(...){
  x <- list(...)
  x[['site_repository']] <- c('https://stat.ethz.ch/CRAN')
  x[['ask']] <- FALSE
  capture.output(do.call(BiocManager::install, x), type= c('message'))
}
