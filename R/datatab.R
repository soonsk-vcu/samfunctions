#' Construct a data dictionary
#'
#' Concats column names and descriptions and outputs a kable table.
#'
#' @import dplyr
#' @import ggplot2
#' @import kableExtra
#' @import gridExtra
#' @param key Vector of column names
#' @param def Vector of descriptions for each column
#' @export
#' @author Samuel Soon
#' @examples
#' convert(c("A","B"), c("Letter A", "Letter B"))

datatab <- function(key,def){
  ret <- data.frame(Name=key,Description=def)
  knitr::kable(ret, caption="Data Table")%>%
    kable_styling(latex_options = "HOLD_position")
}
