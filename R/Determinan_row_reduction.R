
#' Determinan with row reduction
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
#' Determinan_row_reduction(x)
#' @param x squared matrix variable

Determinan_row_reduction <- function(x){

  n <- nrow(x)
  m <- ncol(x)
  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (squared matrix)")

  augmented_mat <- cbind(x)
  cat("matriks awal\n")
  print(x)

  step <- 1

  change <- 0
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
        change = change + 1
      }
    }
  }

  idx <- c()
  index = 1
  for (i in 1:n) {
    pivot <- augmented_mat[i, i]
    if (pivot == 0) {
      stop("tidak dapat diselesaikan")  # System is inconsistent
    }
    else augmented_mat[i, ] <- augmented_mat[i, ] / pivot

    cat("\nStep", step, ": baris", i, "= baris",i, "dibagi", pivot,"\n")
    print(augmented_mat)
    step = step + 1
    idx[index] = pivot
    cat("\nCommon factor yang didapatkan :", idx[index], "\n")
    index = index + 1

    for (j in i:n) {
      if (i != j) {
        ratio <- augmented_mat[j, i] / augmented_mat[i, i]
        if(ratio == 0) next
        augmented_mat[j, ] <- augmented_mat[j, ] - ratio * augmented_mat[i, ]
        cat("\nStep", step," : baris",j, " = ", -1*ratio, "* baris", i, "+ baris", j, "\n")
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  det = 1
  if(change %% 2 == 1) {
    print("karena terjadi pertukaran 2 baris maka determinan dikalikan -1")
    det = -1
    cat("\nDeterminan yang didapatkan =", det, "*",idx[1])
  }
  else {
    det = 1
    cat("\nDeterminan yang didapatkan =",idx[1])
  }


  for(i in 1:(index-1)){
    det <- det * idx[i]
    if(i > 1){
      cat(" *",idx[i])
    }
  }
  cat(" = ",det,"\n")

  return(det)

}
