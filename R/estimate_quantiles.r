##' estimate missing quantiles
##'
##' @param x a vector of quantiles, with levels as names and some missing (NA) values
##' @return a vector of quantiles, with levels as names and all values
##' @importFrom quantgen quantile_extrapolate
##' @author Sebastian Funk
estimate_quantiles <- function(x) {
  vec <- unlist(x)
  if (any(is.na(x))) {
    tau <- as.numeric(names(vec[!is.na(vec)]))
    x[1, ] <-
      quantgen::quantile_extrapolate(
                  tau, vec[!is.na(vec)], as.numeric(names(vec)), nonneg = TRUE, round = TRUE)
  }
  return(x)
}

