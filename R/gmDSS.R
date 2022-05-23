

#'
#' @title Representation of a Gaussian component.
#' @description Represents a set of normally-distributed points in R^n
#'     by their mean vector, covariance matrix and number of data points.
#' @field mu: [vector] mean vector.
#' @field Sigma: [matrix] covariance matrixn
#' @field size: [int] sample sizen
#'
Gaussian <- setRefClass("Gaussian", fields = list(
    mu = "numeric",
    Sigma = "matrix",
    size = "numeric"
))

#'
#' @title Representation of a Gaussian Mixture.
#' @description Just a wrapper on a list of Gaussian - to better understand what is in the list.
#' @field components: list of Gaussian components.
#'
Mixture <- setRefClass("Mixture", fields = list(
    components = "list"   # list of Gaussian
))


#'
#' @title Find the components of a Gaussian mixture.
#' @description Uses mixtools' EM algorithm to find the components,
#'     then cast the result to a Mixture to not leak important data.
#' @param X: [data.frame] the data points, one per line. Columns are measured variables.
#' @param k: [int] the expected number of Gaussian components in the mixture.
#' @return [Mixture] a list of Gaussians.
#'
gmDSS <- function(X, cols, K) {
    cols <- .decode.arg(cols)
    k <- .decode.arg(K)
    if (!is.null(cols)){
        X = X[,cols]
    }
    X <- .split.numerics(X)$numerics
    model = mixtools::mvnormalmixEM(X, k=k);
    return(model.to.Mixture(model))
}


#'
#' @title Cast the output of mvnormalmixEM to a Mixture object so that we don't
#'     send unwanted data to the client.
#' @description We keep only the mean vector, covariance matrix, and number of data points.
#'
model.to.Mixture <- function(model) {
    K = length(model$lambda)  # number of components
    components = vector("list", K)
    maxprob = apply(model$posterior, 1, max)
    # For each row, mark the index of the component that has the max probability
    max.posterior.idx = apply(model$posterior, MARGIN=1, FUN=which.max)
    for (k in 1:K) {
        components[[k]] = Gaussian(
            mu = model$mu[[k]],
            Sigma = model$sigma[[k]],
            size = sum(max.posterior.idx == k)
        )
    }
    return(Mixture(components = components))
}


