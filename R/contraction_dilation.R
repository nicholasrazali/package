
#' Mencari Kontraksi dan Dilatasi dari vektor R2 atau Vektor R3
#' @description
#' Fungsi ini akan melakukan pencarian kontraksi dan dilatasi dari vektor
#' berdasarkan besaran faktor skalar k.
#' @details
#' Fungsi ini untuk melakukan pencarian kontraksi dan dilatasi dari vektor
#' berdasarkan besaran faktor skalar k. Jika besaran k berada di antara 0 dan 1
#' maka dilakukan transformasi kontraksi, sedangkan jika besaran k lebih besar
#' atau sama dengan 1 maka dilakukan transformasi dilatasi. Jika data yang dimasukan berjumlah 2 maka
#' dilakukan untuk vektor R2, jika berjumlah 3 maka dilakukan vektor R3.
#' @examples
#' A = c(3,4)
#' k = 3
#' contraction_dilatation(A, k)
#' @param A vektor input yang ingin dilakukan transformasi kontraksi dan dilatasi
#' @param k besaran skalar k
#' @return Hasil transformasi kontraksi dan dilatasi
#' @export

contraction_dilatation <- function(A, k){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)<2 || length(A)>3) stop("jumlah data vektor A harus terdiri dari 2 atau 3 data")
  else if(is.numeric(k) == FALSE || k < 0) stop("Variabel alfa harus dalam angka dan harus lebih besar dari 0")

  transformasi = diag(length(A)) * k

  cat("Vektor A: \n")
  print(A)

  cat("\nTransformasi matriks : \n")
  print(transformasi)

  cat("\nTransformasi Matriks * vektor A : \n")

  transformation = transformasi %*% A

  return(transformation)
}
