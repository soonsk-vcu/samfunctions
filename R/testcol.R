#' make column of data frame from test results, with rounded numbers
#'
#' Returns a vector of test parameters
#'
#' @param l test results given in list form
#' @param string_data default TRUE. Decides whether string test results are returned.
#' @export
#' @author Samuel Soon
#'
#'


testcol <- function(l, string_data = TRUE){
  # separate numeric and string entries for tidying
  numind <- l[sapply(l, is.numeric)]
  numdata <- round(unlist(l[sapply(l, is.numeric)]), 4)
  strdata <- unlist(l[!sapply(l, is.numeric)])
  #(strdata)
  # paste formatted entries with dependent variable name for clarity
  if(string_data){
    c(numdata, strdata)
  }else{
    numdata
  }
}
