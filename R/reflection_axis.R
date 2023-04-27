
#' Mencari Refleksi dari vektor R2
#' @description
#' Fungsi ini akan melakukan pencarian refleksi dari vektor
#' berdasarkan pada axis ataupun garis.
#' @details
#' Fungsi ini untuk melakukan pencarian refleksi dari vektor
#' berdasarkan pada axis ataupun garis. Jika axis bernilai 'x', maka
#' akan dilakukan refleksi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan refleksi terhadap sumbu y, sedangkan jika axis bernilai
#' 'line' maka akan dilakukan refleksi terhadap line y = x.
#' @examples
#' A = c(3,4)
#' axis = "x"
#' reflection_axis(A, axis)
#' @param A vektor input yang ingin dilakukan transformasi refleksi
#' @param axis sumbu refleksi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi refleksi
#' @export

reflection_axis <- function(A, axis){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=2) stop("jumlah data vektor A harus terdiri dari 2 data")

  if (axis == "x") {
    reflect<- matrix(c(1, 0, 0, -1), 2, 2)
  } else if (axis == "y") {
    reflect <- matrix(c(-1, 0, 0, 1), 2, 2)
  } else if (axis == "line") {
    reflect <- matrix(c(0, 1, 1, 0), 2, 2)
  } else {
    stop("Kesalahan input axis, pilih antara 'x', 'y', atau 'line'.")
  }

  cat("Vektor A: \n")
  print(A)

  cat("\nRefleksi matriks : \n")
  print(reflect)

  cat("\nRefleksi Matriks * vektor A : \n")

  reflection = reflect %*% A

  return(reflection)
}
