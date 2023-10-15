#' make shapiro table
#'
#' Returns a table where columns are results of shapiro.test using preferred alternative and method.
#'
#' @param df dataframe with only numeric data
#' @export
#' @author Samuel Soon
#'
#'


shapirotable <- function(df){

  makecol <- function(col){
    x <- unlist(col)
    # test
    l <- shapiro.test(x)
    # separate numeric and string entries for tidying
    numind <- l[sapply(l, is.numeric)]
    numdata <- round(unlist(l[sapply(l, is.numeric)]), 4)
    strdata <- unlist(l[!sapply(l, is.numeric)])
    # paste formatted entries with dependent variable name for clarity
    cortests <- c(numdata)
    cortests
  }


  data.frame(mapply(makecol,df))
}
