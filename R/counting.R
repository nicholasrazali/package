
#' Creates counting
#' @description
#' `sum` returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#' @export
#' @examples
#' counting(10, 3)
#' counting(20, 5)
#' @param x numeric variable
#' @param y numeric variable
counting <- function(x, y){

  sum = x+y
  min = x-y
  return(list("sum" = sum, "minus" = min))
}
