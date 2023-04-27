
#' Check Apakah Matriks dapat dilakukan Diagonalisasi
#' @description
#' Fungsi ini akan menentukan apakah matriks dapat dilakukan diagonalisasi atau tidak.
#'
#' @details
#' Fungsi ini untuk menentukan apakah matriks dapat dilakukan diagonalisasi atau tidak.
#' @examples
#' A = matrix(c(2,-1,0,-1,2,0,0,0,3),3,3)
#' check_diagonalizable(A)
#' @param A variable input matriks
#' @export

check_diagonalizable <- function(A){
  eig = eigen(A)
  p = eig$vectors
  tryCatch(solve(p), error = function(e){
    stop("Matriks tidak dapat dilakukan diagonalisasi")
  })
  cat("Eigen Values = \n")
  print(eig$values)

  cat("\nEigen Vektor atau matriks P = \n")
  print(eig$vectors)

  check = round(solve(p)%*%A%*%p)
  cat("\nMatriks P^-1 * A * P : \n")
  print(check)

  check2 = diag(eig$values)
  cat("\nMatriks diagonal eigen values : \n")
  print(check2)

  for (i in 1:nrow(check)) {
    for (j in 1:nrow(check)) {
      if(as.integer(check[i,j]) != as.integer(check2[i,j])) stop("Matriks tidak dapat dilakukan diagonalisasi")
    }
  }
  print("Matriks dapat dilakukan diagonalisasi")
}
