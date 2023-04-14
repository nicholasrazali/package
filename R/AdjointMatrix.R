
#' Determinan with cofactor method
#' @description
#' 'Determinan' dengan menggunakan metode row reduction secara bertahap
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#' @export
#' @examples
#' x <- matrix(c(0,3,2,1,-6,6,5,9,1),3,3)
#' AdjointMatrix(x)
#' @param x squared matrix variable

AdjointMatrix <- function(mat) {
  n <- nrow(mat)

  if (n != ncol(mat)) {
    stop("Input matrix must be square.")
  }

  adj_mat <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      sub_mat <- mat[-i, -j]
      cofactor = (-1)^(i+j) * det(sub_mat)
      cat(sprintf("cofactor %d,%d",j,i)," =",(-1)^(i+j),"*",det(sub_mat),"=",cofactor,"\n")
      adj_mat[j, i] <- cofactor
    }
  }
  cat("\n")

  return(adj_mat)
}

