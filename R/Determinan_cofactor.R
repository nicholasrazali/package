
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
#' Determinan_cofactor(x)
#' @param x squared matrix variable

Determinan_cofactor <- function(matrix, depth = 0) {
  n <- nrow(matrix)
  if(n != ncol(matrix)) {
    stop("Matrix must be square")
  }
  if(n == 1) {
    cat(paste0(rep(" ", depth * 2), "Returning determinant ", matrix[1,1], "\n"))
    return(matrix[1,1])
  } else if(n == 2) {
    det <- matrix[1,1] * matrix[2,2] - matrix[1,2] * matrix[2,1]
    cat(paste0(rep(" ", depth * 2), "Returning determinant ", det, "\n"))
    return(det)
  } else {
    det <- 0
    for(j in 1:n) {
      minor <- matrix[-1, -j]
      cat("\nMinor\n")
      print(minor)
      cofactor <- (-1)^(1+j) * Determinan_cofactor(minor, depth + 1)
      term <- matrix[1,j] * cofactor
      det <- det + term
      cat(paste0(rep(" ", depth * 2), "Adding term ", term, " from element ", 1, ",", j, "\n"))
    }
    cat(paste0(rep(" ", depth * 2), "Returning determinant ", det, "\n"))
    return(det)
  }
}
