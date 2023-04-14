
#' Counting matrix
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
#' x <- matrix(1:4,2,2)
#' y <- matrix(5:8,2,2)
#' counting(x,y)
#' @param x numeric variable
#' @param y numeric variable


Counting <- function(x, y){
  if(!is.matrix(x) || !is.matrix(y)){
    stop("data must be in matrix")
  }
  nrow_x = nrow(x)
  ncol_x = ncol(x)

  nrow_y = nrow(y)
  ncol_y = ncol(y)

  check  = dim(x) == dim(y)

  if(check[1] & check[2]){
    sum = x+y
    min = x-y
  }
  else{
    sum = "cannot do sum because different dimension"
    min = "cannot do minus because different dimension"
  }

  if(ncol_x == nrow_y){
    multiply = x %*% y
  }
  else multiply = "cannot do multiplication"

  return(list("sum" = sum, "minus" = min, "multiplication" = multiply))
}
