#' pythagoras
#'
#' Computes third side of right triangle given two other sides
#'
#' @param a first side
#' @param b second side
#' @param c hypotenuse side
#' @export
#' @author Samuel Soon
#' @examples
#' pythagoras(3,4)
#'

pythagoras <- function(a=0,b=0,c=0){
  # parameter typing and size check
  args <- c(a,b,c)
  if(!is.numeric(args) | sum(as.logical(args)) != 2){
    stop("Expected 2 numeric parameters")
  }

  # compute missing side
  sqrt(abs(c^2 - a^2 - b^2))
}
