
#' Mencari Rotasi dari vektor R2
#' @description
#' Fungsi ini akan melakukan pencarian rotasi dari vektor
#' berdasarkan besaran sudut pada dua dimensi.
#' @details
#' Fungsi ini untuk melakukan pencarian rotasi dari vektor
#' berdasarkan besaran sudut pada dua dimensi.
#' @examples
#' A = c(3,4)
#' alfa = 60
#' rotation_axis(A, alfa = alfa )
#' @param A vektor input yang ingin dilakukan transformasi rotasi
#' @param alfa besaran sudut
#' @return Hasil transformasi rotasi
#' @export

rotation_axis <- function(A, alfa){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=2) stop("jumlah data vektor A harus terdiri dari 2 data")
  else if(is.numeric(alfa) == FALSE) stop("Variabel alfa harus dalam angka")

  radian <- alfa * pi / 180
  rotasi <- matrix(c(cos(radian), sin(radian), -sin(radian), cos(radian)), 2, 2)

  cat("Vektor A: \n")
  print(A)

  cat("\nRotasi matriks : \n")
  print(rotasi)

  cat("\nRotasi Matriks * vektor A : \n")

  rotation = rotasi %*% A

  return(rotation)
}
