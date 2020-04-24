scaleDSS <- function(what, center = TRUE, scale = TRUE){
  center <- .decode.arg(center)
  scale <- .decode.arg(scale)
  sp <- .split.numerics(what)
  sc <- as.data.frame(scale(sp$numerics, center = center, scale = scale))
  cbind(sp$other,sc)
}
