#' @export
selfUpgrade <- function(other.package = NULL ,method = NULL, lib = NULL, extra = NULL, version = NULL, verbose = FALSE, only.other = FALSE ){
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
  if(!is.null(extra)){
    options(download.file.extra = extra)
  }
  if(!only.other || !is.null(other.package)){
  x <- list( dsSwissKnife = capture.output(install.packages('dsSwissKnife', lib = lib,
                                                            repos=c("https://sophia-fdb.vital-it.ch/SIB-R", 'https://sophia-fdb.vital-it.ch/CRAN', "https://sophia-fdb.vital-it.ch:8443/SIB-R", 'https://sophia-fdb.vital-it.ch:8443/CRAN'),
                                         method = method), type = c('message')))
  } else {
    x <- list()
  }
  if(!is.null(other.package)){
    if (!is.null(version)){
      if(other.package %in% c('dsBase')){
        x[[other.package]] <- capture.output(install.packages(paste0('https://sophia-fdb.vital-it.ch:8443/CRAN-DS/src/contrib/Archive/', other.package, '/',version), lib = lib,
                                                              repos=NULL, type = 'source', method = method),type = c('message'))

      } else {
        x[[other.package]] <- capture.output(install.packages(paste0('https://sophia-fdb.vital-it.ch:8443/CRAN/src/contrib/Archive/', other.package, '/',version), lib = lib,
                                                            repos=NULL, type = 'source', method = method),type = c('message'))
      }
    } else {

          x[[other.package]] <- capture.output(install.packages(other.package, lib = lib,
                                                          repos=c('https://sophia-fdb.vital-it.ch/SIB-R',  'https://sophia-fdb.vital-it.ch/CRAN', "https://sophia-fdb.vital-it.ch:8443/SIB-R", 'https://sophia-fdb.vital-it.ch:8443/CRAN',  'https://sophia-fdb.vital-it.ch:8443/CRAN-DS'),
                                                          method = method),type = c('message'))
    }
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
  capture.output(do.call(BiocManagesr::install, x), type= c('message'))
}


.install_old_version <- function(urls, lib, method){
  install.packages(paste0('https://sophia-fdb.vital-it.ch/CRAN/src/contrib/Archive/', other.package, '/',version), lib = lib,
                   repos=NULL, type = 'source', method = method)
}
