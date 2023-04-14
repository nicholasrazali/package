
#' Invers with row reduction
#' @description
#' 'invers' dengan menggunakan metode row reduction secara bertahap
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the [Summary()] group generic. For this to work properly,
#' the arguments `...` should be unnamed, and dispatch is on the
#' first argument.
#' @export
#' @examples
#' x <- matrix(1:4,2,2)
#' invers_row_reduction(x)
#' @param x squared matrix variable


invers_row_reduction <- function(x) {
  n <- nrow(x)
  m <- ncol(x)
  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (squared matrix)")

  augmented_mat <- cbind(x, diag(n))
  cat("matriks awal\n")
  print(x)

  cat("\nStep 1 : " ,"Menambahkan matrix identity di sebelah kanan\n")
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

  inv <- augmented_mat[, -c(1:n)]

  ' handling jika Na atau inf
  for(i in 1:n){
    for(j in 1:n){
      if(is.nan(inv[i,j]) || is.infinite(inv[i,j]))
        stop("Invers dari matrix tidak dapat dihasilkan")
    }
  }
  '

  cat("\n Hasil Inverse dengan row reduction \n")
  return(inv)
}
