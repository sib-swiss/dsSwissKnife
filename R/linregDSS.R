#'
#' @title Returns the quantities shared to the client to performed distributed linear regression.
#' @description Caclulates X'X and X'y as in the normal equation, i.e. the MLE for beta
#'     is {{ b = inv(X'X) X'Y }}. No need to calculate beta here, nor to share the matrix X.
#'     Also returns y'y  and the sample size
#'     in order to recompute the T statistics (and the p-values).
#' @param dep_var: [string] the column name for the dependent variable (y).
#' @param expl_vars: [vector of strings] the column names for the explanatory variables.
#' @return a LinearModel.
#'
linregDSS <- function(df, dep_var, expl_vars){

    dep_var <- .decode.arg(dep_var)
    expl_vars <- .decode.arg(expl_vars)
    # If no expl_vars are given, take them all except dep_var
    if (is.null(expl_vars)){
        tryCatch({
            by.col <- get('by.col', envir = .mycache)
            expl_vars = setdiff(colnames(df), c(by.col, dep_var))
        }, error = function(e){stop('Run init first')})
    }

    # Coerce factors to numeric
    df = as.data.frame(sapply(colnames(df), FUN = function(col) {
        return(df[,col])
    }))

    n = nrow(df)
    y = df[, dep_var]
    X = as.matrix(df[, expl_vars])
    X = cbind(rep(1, n), X)         # Add a column of 1s
    XX = crossprod(X)               # t(X) %*% X
    Xy = crossprod(X, y)            # t(X) %*% y
    yy = as.numeric(crossprod(y))   # t(y) %*% y

    res = list(XX=XX, Xy=Xy, yy=yy, size=n)  # SharedLinearModel
    return(res)
}

