
#' @export
#'
partialKmeans <- function(whatname, centers, means = NULL, membership = FALSE, split = FALSE, iter.max = NULL,  nstart = NULL, algorithm = 'Forgy'){
  if(!exists('mems', where = .GlobalEnv)){
      assign('mems', list(), envir = .GlobalEnv)
  }
  assign('mems', c(mems, .get_memory_usage()), envir = .GlobalEnv)
  save(mems, file = 'mem.rda')
  centers <- .decode.arg(centers)
  what <- get(whatname, envir = parent.frame())
  allowed <- setdiff(colnames(what), get('hidden', envir = .mycache))
  what <- .split.numerics(what[,allowed])$numerics
  if(split){  # just apply kmeans and get out
    if(length(centers) == 1L) { # it's a number
      k <- centers
    } else {
      centers <- as.matrix(centers)
      k <- nrow(centers)
    }
    km.name <- paste0('km_clust', k)
    km <- suppressWarnings( kmeans(x = what, centers = centers, iter.max = iter.max,  nstart = nstart, algorithm = .decode.arg(algorithm)) ) # don't want 'empty cluster' or 'did not converge'
    #create a vector with cluster membership
    cluster.membership <- as.vector(fitted(km, 'classes'))
    #set attribute to allow download
    attr(cluster.membership, 'download_allowed') <- TRUE
    assign(paste0(whatname,'_', km.name), cluster.membership, envir = parent.frame())
    return(km)
  }
  if(membership) {

    centers <- as.matrix(centers)
    km.name <- paste0('km_clust', nrow(centers))
    #    cluster.membership <-  factor(apply(what, 1, function(x){
    # not very efficient: loop through the points, calculate each point's distance to the centers,
    # maybe implement this in C at some point

    #      xx <- as.matrix(dist(rbind(x,centers)))
    #      yy <- xx[rownames(centers),setdiff(colnames(xx), rownames(centers))]^2
    #      which.min(yy)

    #    }))
    cluster.membership <- factor(apply(what, 1, function(x){
      .shortest_distance(x, centers)$index
    }))
    #set attribute to allow download
    if(.not.enough.members(cluster.membership)){
      return(FALSE)
    }
    attr(cluster.membership, 'download_allowed') <- TRUE
    #create a downloadable vector with cluster membership

    assign(paste0(whatname,'_', km.name), cluster.membership, envir = parent.frame())

    return(TRUE)
  }

  if (!is.null(means)){
    means <- .decode.arg(means)
    # this is the last iteration; calculate global sums of squares and get out
    km <- list()
    # total sum of squares to the global center
    km$totss <- sum(scale(what, center = means, scale = FALSE )^2)

    # get the closest center to each point:
    centers <- as.matrix(centers)
    km$withinss <- c()
    km$withinss[rownames(centers)] <- rep(0,nrow(centers))


    apply(what, 1, function(x){
      this.center <- .shortest_distance(x, centers)
      km$withinss[this.center$index] <<- km$withinss[this.center$index] + this.center$value
    })
    km$tot.withinss <- sum(km$withinss)
    km$betweenss <- km$totss - km$tot.withinss

    return(km)
  }

  km <- suppressWarnings( kmeans(x = what, centers = centers, iter.max = 1, algorithm = 'Forgy') ) # don't want 'empty cluster' or 'did not converge'

  km$cluster <- table(km$cluster) #doesn't make sense to keep the atomic data

  km$centers[is.na(km$centers)] <- 0
  #rownames(km$centers) <- rownames(centers)
  rownames(km$centers) <- sub('c','', rownames(km$centers), fixed=TRUE)
  #print(km$centers)
  km$centers <- km$centers[order(rownames(km$centers)),]
  km$cluster[setdiff(rownames(km$centers), names(km$cluster))] <- 0
  km$cluster <- km$cluster[order(names(km$cluster))]

  return(km)
}

.shortest_distance <- function(point, centers){
  xx <- as.matrix(dist(rbind(point,centers)))
  yy <- xx[rownames(centers),setdiff(colnames(xx), rownames(centers))]^2
  ind <- which.min(yy)
  list(index = ind, value = yy[ind])
}

.not.enough.members <- function(x){
  thresh <- .dsBase_setFilterDSS()
  counts <- table(x)
  return(any(counts <= thresh))
}
