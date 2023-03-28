
#' Multiplication Matrix with Step
#' @description
#' `MultiplicationbyStep` returns the the step and resut of multiplication two matrix.
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
#' MultiplicationbyStep(x,y)
#' @param x numeric variable
#' @param y numeric variable


MultiplicationbyStep <- function(x, y){
  if(!is.matrix(x) || !is.matrix(y)){
    stop("data must be in matrix")
  }
  nrow_x = nrow(x)
  ncol_x = ncol(x)

  nrow_y = nrow(y)
  ncol_y = ncol(y)

  if(ncol_x != nrow_y){
    stop("number of column in x and number of row in y must be same")
  }
  else{
    data = rep(0,nrow_x*ncol_y)
    temp = matrix(data,nrow_x,ncol_y)
    temp2 = matrix(data,nrow_x,ncol_y)
    temp3 = matrix(data,nrow_x,ncol_y)
    res = 0
    res2 = 0
    res3 = 0
    count = 0
    count2 = 0
    for (i in 1:nrow_x) {
      for (j in 1:ncol_y) {
        for (k in 1:nrow_y) {
          res = res + x[i,k]*y[k,j]
          res2 = paste(x[i,k],y[k,j],sep = "*")
          count = x[i,k]*y[k,j]
          if(k==1){
            res3 = res2
            count2 = count
          }else {
            res3 = paste(res3 , res2, sep = " + ")
            count2 = paste(count2, count, sep = " + ")
          }
        }
        temp[i,j] = res
        temp2[i,j] = res3
        temp3[i,j] = count2
        count = 0
        count2 = 0
        res2 = 0
        res3 = 0
        res = 0
      }
    }
  }
  return(list("step" = list(temp2,temp3), "hasil" = temp))
}
