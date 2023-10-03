#' Convert data column values based on key and value
#'
#' Convert vector based on dictionary. Can handle letter mutations if a single word is passed in. Interval is used when numeric ranges are given to from.
#' In this case, length(from) should be length(to) - 1 for best results
#' (but not necessarily necessary), and from should represent partition bounds.
#' Intervals are not strict. The function can also convert to different data types.
#'
#' @import dplyr
#' @import ggplot2
#' @import kableExtra
#' @import gridExtra
#' @param lst Data column to be converted
#' @param from Keys for dictionary as a vector
#' @param to Values for dictionary as a vector
#' @param interval Set to TRUE if converting based on partition bounds. Recommended to have length(from) = length(to)-1
#' @export
#' @author Samuel Soon
#' @examples
#' convert(10:1, 1:10, letters[1:10])


convert <- function(lst, from, to, interval=FALSE){
  isString <- FALSE
  # if converting letters within single string, do a split to individual chars
  if(typeof(lst) == "character" & length(lst) == 1){
    vec <- (strsplit(lst,""))[[1]]
    isString <- TRUE
  }else{
    vec <- lst
  }

  # auxilary function to convert object to corresponding dictionary entry
  aux <- function(x, from, to){
    ind <- ifelse(interval, which(from >= x)[1], which(from == x))
    ifelse(length(ind) > 0  & !is.na(ind),to[ind],
           ifelse(interval, to[length(to)], x))
  }


  ret <- sapply(vec, aux, from=from, to=to)
  if(isString){
    ret <- paste(ret, collapse = "")
  }
  ret
}
