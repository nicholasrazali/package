
#' Invers dengan Row Reduction
#' @description
#' Fungsi ini akan melakukan pencarian nilai invers dari suatu matriks persegi dengan menggunakan metode row reduction dan disertai dengan langkah pengerjaannya.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai invers dengan menggunakan metode row reduction, Metode
#' row reduction dilakukan dengan cara transformasi matriks menjadi matriks echelon baris dan kemudian
#' menerapkan subtitusi mundur untuk mendapatkan nilai invers.
#'
#' @examples
#' x <- matrix(1:4,2,2)
#' invers_row_reduction(x)
#' @param x matriks persegi
#' @return Nilai invers dari matriks
#' @export

invers_row_reduction <- function(x) {
  if(is.matrix(x) == FALSE) stop("variabel x harus dalam matriks")
  n <- nrow(x)
  m <- ncol(x)

  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")

  cat("Matriks awal\n")
  print(x)

  augmented_matrix <- cbind(x, diag(n))
  cat("\nStep 1: Menambahkan matriks identitas di sebelah kanan\n")
  print(augmented_matrix)

  step <- 2

  for (i in 1:n) {
    pivot <- augmented_matrix[i, i]
    if (pivot == 0) {
      if(i == n) {
        return(cat("\n\nMatriks tidak mempunyai invers"))
      }
      else{
        for (j in (i+1):n) {
          if (abs(augmented_matrix[j, i]) != 0) {
            augmented_matrix[c(i, j), ] <- augmented_matrix[c(j, i), ]
            cat(paste0("\nStep ", step ,": tukar baris ",i, " dengan baris ",j, "\n"))
            print(augmented_matrix)
            step = step + 1
            break
          }
          if (j == n) {
            return(cat("\n\nMatriks tidak mempunyai invers"))
          }
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

  inv <- augmented_matrix[, -c(1:n)]

  cat("\n\nHasil inverse dengan row reduction \n")
  return(inv)
}
