#' Turnbull nonparametric estimator.
#' 
#' Turnbull's non-parametric estimator. Does not handle interval-censored data,
#' for which you should can the package \code{gte}.
#' 
#' @export
#' 
#' @param formula formula used to tabulate.
#' @param data the data set.
#' @return a numeric value of the estimator.
turnbull <- function(formula, data) {
  x <- xtabs(formula, data = data)
  values <- as.numeric(names(x))
  h <- sapply(1:length(x), function(i) {
    v0 <- ifelse(i==1, 0, values[i-1])
    (values[i] - v0)*x[[i]]
  })
  sum(h)
}