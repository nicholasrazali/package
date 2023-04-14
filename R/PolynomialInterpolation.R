
#' Polynomial Interpolation
#' @description
#' 'Polynomial Interpolation' dengan secara bertahap
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#' @export
#' @examples
#' x <- c(1,2,3,4)
#' y <- c(3,-2,-5,0)
#' PolynomialInterpolation(x,y)
#' @param x squared matrix variable
#' @param y vector variable


PolynomialInterpolation <- function(x, y) {
  len_x <- length(x)
  len_y <- length(y)
  if(len_x != len_y) stop("Jumlah x dan y harus sama")

  A <- matrix(rep(1,len_x),len_x,1)

  for(i in 1:(len_x - 1)){
    A <- cbind(A, as.matrix(x^i))
  }
  n <- nrow(A)
  augmented_mat   <- cbind(A, y)
  cat("matriks awal\n")
  print(A)

  cat("\nStep 1 : " ,"Menjadikan Augmented Matrix\n")
  print(augmented_mat)
  step <- 2

  for (i in 1:n) {
    # Find the pivot row
    pivot_row <- i
    if(augmented_mat[i,i] == 0){
      for (j in i:n) {
        if (abs(augmented_mat[j, i]) > abs(augmented_mat[pivot_row, i])) {
          pivot_row <- j
        }
      }
      # Swap rows if necessary
      if (pivot_row != i) {
        augmented_mat[c(i, pivot_row), ] <- augmented_mat[c(pivot_row, i), ]
        cat("\nStep", step ,": tukar baris ",i, "dengan baris ",pivot_row, "\n")
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  for (i in 1:n) {
    pivot <- augmented_mat[i, i]
    if (pivot == 0) {
      stop("tidak dapat diselesaikan")  # System is inconsistent
    }
    else augmented_mat[i, ] <- augmented_mat[i, ] / pivot
    if(pivot != 1){
      cat("\nStep", step, ": baris ", i, " = baris ",i, " dibagi ", pivot,"\n")
      print(augmented_mat)
      step = step + 1
    }
    for (j in i:n) {
      if (i != j) {
        ratio <- augmented_mat[j, i] / augmented_mat[i, i]
        if(ratio == 0) next
        augmented_mat[j, ] <- augmented_mat[j, ] - ratio * augmented_mat[i, ]
        cat("\nStep", step," : baris ",j, " = ", -1*ratio, "* baris ", i, " + baris ", j, "\n")
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  for (i in (n-1):1) {
    for (j in (i+1):n) {
      if (i != j) {
        ratio <- augmented_mat[i, j] / augmented_mat[j, j]
        if(ratio == 0) next
        augmented_mat[i, ] <- augmented_mat[i, ] - ratio * augmented_mat[j, ]
        cat("\nStep", step," : baris ",i, " = ", -1*ratio, "* baris ", j, " + baris ", i, "\n")
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  x <- augmented_mat[, n + 1]
  cat("\n Hasil persamaan yang didapatkan \n")
  return(x)
}
