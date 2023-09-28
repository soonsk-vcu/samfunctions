#' Summarize count data based on group and filter
#'
#' Takes a data table, filtering expression, grouping column, and outputs count data
#'
#' @import dplyr
#' @import ggplot2
#' @import kableExtra
#' @import gridExtra
#' @param tab Data frame
#' @param exp Logical expression for inserting into filter function
#' @param group Column name for grouping, surrounded by quotations
#' @export
#' @author Samuel Soon
#' @examples
#' getCountData(lacrime, Age==35, "Date OCC")


getCountData <- function(tab, exp, group){
  tab %>% filter({{exp}}) %>% group_by(!!sym(group)) %>% summarize(n=n())
}
