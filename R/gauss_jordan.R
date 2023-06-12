
#' Eliminasi Gauss-Jordan
#' @description
#' Fungsi ini akan melakukan elimanasi Gauss-Jordan untuk menyelesaikan sistem persamaan linear yang disertai dengan langkah pengerjaannya.
#'
#' @details
#' Fungsi ini untuk melakukan penyelesaian sistem persamaan linear dengan menggunakan eliminasi Gauss-Jordan.
#' Proses eliminasi Gauss-Jordan dilakukan dengan mengubah matriks koefisien menjadi matriks augmented,
#' kemudian melakukan operasi baris elementer hingga matriks tersebut menjadi matriks echelon baris,
#' lalu mengubah matriks tersebut menjadi matriks reduksi baris.
#' @examples
#' A <- matrix(c(1,1,0,0,-1,2,1,0,1),3,3)
#' b <- c(4,-1,7)
#' gauss_jordan(A,b)
#' @param A variabel matriks persegi
#' @param b variabel vektor
#' @return Penyelesaian persamaan linear
#' @export

gauss_jordan <- function(A, b) {
  if(is.matrix(A) == FALSE) stop("variabel A harus dalam matriks")
  else if(is.vector(b) == FALSE) stop("variabel b harus dalam vector")

  n <- nrow(A)
  m <- ncol(A)

  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")
  else if(n != length(b)) stop("jumlah vector b tidak sama dengan jumlah baris dan kolom pada matriks")

  cat("Matriks awal\n")
  print(A)

  augmented_matrix   <- cbind(A, b)

  cat("\nStep 1: Menjadikan Augmented Matriks\n")
  print(augmented_matrix)
  step <- 2

  for (i in 1:n) {
    pivot <- augmented_matrix[i, i]
    if (pivot == 0) {
      if(i == n) return(cat("\n\nTerdapat banyak solusi"))
      else{
        for (j in (i+1):n) {
          if (abs(augmented_matrix[j, i]) != 0) {
            augmented_matrix[c(i, j), ] <- augmented_matrix[c(j, i), ]
            cat(paste0("\nStep ", step ,": tukar baris ",i, " dengan baris ",j, "\n"))
            print(augmented_matrix)
            step = step + 1
            break
          }
          if (j == n) return(cat("\n\nTerdapat banyak solusi"))
        }
      }
    }
    pivot <- augmented_matrix[i, i]
    augmented_matrix[i, ] <- augmented_matrix[i, ] / pivot
    if(pivot != 1){
      cat(paste0("\nStep ", step, ": baris ", i, " = baris ",i, " dibagi ", pivot,"\n"))
      print(augmented_matrix)
      step = step + 1
    }
    for (j in i:n) {
      if (i != j) {
        ratio <- augmented_matrix[j, i] / augmented_matrix[i, i]
        if(ratio == 0) next
        augmented_matrix[j, ] <- augmented_matrix[j, ] - ratio * augmented_matrix[i, ]
        cat(paste0("\nStep ", step,": baris ",j, " = ", -1*ratio, " * baris ", i, " + baris ", j, "\n"))
        print(augmented_matrix)
        step = step + 1
      }
    }
  }
  for (i in (n-1):1) {
    for (j in (i+1):n) {
      if (i != j) {
        ratio <- augmented_matrix[i, j] / augmented_matrix[j, j]
        if(ratio == 0) next
        augmented_matrix[i, ] <- augmented_matrix[i, ] - ratio * augmented_matrix[j, ]
        cat(paste0("\nStep ", step,": baris ",i, " = ", -1*ratio, " * baris ", j, " + baris ", i, "\n"))
        print(augmented_matrix)
        step = step + 1
      }
    }
  }
  x <- augmented_matrix[, n + 1]
  cat("\n\nHasil persamaan yang didapatkan \n")
  return(x)
}
