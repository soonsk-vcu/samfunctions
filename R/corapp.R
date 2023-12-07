#' make a correlation p-value matrix
#'
#' returns a matrix of correlation test p-values from cor.test
#'
#'
#' @param df purely numerical data frame
#' @param alternative parameter to be passed to cor.test
#' @param method parameter to be passed to cor.test
#' @export
#' @author Samuel Soon
#'
#'

cortesttable <- function(df, alternative = NULL, method = "Spearman") {
  corapp <- function(x, y) {
    ret <- c()
    for (i in 1:ncol(y)) {
      ret <- c(ret, cor.test(x, unlist(y[, i]))$p.value)
    }
    ret
  }
  sapply(numdat,corapp, y=numdat)
}
