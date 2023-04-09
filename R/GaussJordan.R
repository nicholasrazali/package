
#' Gauss Jordan
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
#' A <- matrix(c(1,1,0,0,-1,2,1,0,1),3,3)
#' b <- c(4,-1,7)
#' GaussJordan(A,b)
#' @param A squared matrix variable
#' @param b vector variable


GaussJordan <- function(A, b) {
  n <- nrow(A)
  augmented_mat   <- cbind(A, b)
  cat("matriks awal\n")
  print(A)

  cat("\nStep 1 : " ,"Menambahkan vector kanan\n")
  print(augmented_mat)
  step <- 2

  for (i in 1:n) {
    # Find the pivot row
    pivot_row <- i
    if(augmented_mat[pivot_row,pivot_row] == 0){
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
      cat("\nStep", step, ": baris ", i, " dibagi ", pivot,"\n")
      print(augmented_mat)
      step = step + 1
    }
    for (j in 1:n) {
      if (i != j) {
        ratio <- augmented_mat[j, i] / augmented_mat[i, i]
        if(ratio == 0) next
        augmented_mat[j, ] <- augmented_mat[j, ] - ratio * augmented_mat[i, ]
        cat("\nStep", step, ": baris ", j, "- ", ratio, "* baris ", i,"\n")
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  x <- augmented_mat[, n + 1]
  cat("\n Hasil persamaan yang didapatkan \n")
  return(x)
}
