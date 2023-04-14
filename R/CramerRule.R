
#' Polynomial Interpolation
#' @description
#' 'Gauss Jordan' dengan secara bertahap
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#' @export
#' @examples
#' A <- matrix(c(1,-3,-1,0,4,-2,2,6,3),3,3)
#' b <- c(6,30,8)
#' CramerRule(A,b)
#' @param A squared matrix variable
#' @param b vector variable

CramerRule <- function(A,b){
  n <- nrow(A)
  m <- ncol(A)
  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (squared matrix)")

  cat("Matriks awal A\n")
  print(A)

  det <- det(A)
  cat("\nDeterminan matriks awal = ", det, "\n")

  dett <- c()
  for(i in 1:m){
    temp = A
    temp[,i] = b
    cat("\nMatriks A",i,"\n")
    print(temp)
    dett[i] = det(temp)
    cat("\nDeterminan matriks A",i," = ", dett[i], "\n")
  }

  x <- c()
  for(i in 1:m){
    x[i] = dett[i]/det
    cat("\nX",i," = ",dett[i],"/",det," = ", x[i])
  }
  cat("\n\n")
  return(x)
}
