
#' Mencari Refleksi dari vektor R3
#' @description
#' Fungsi ini akan melakukan pencarian refleksi dari vektor
#' berdasarkan pada plane.
#' @details
#' Fungsi ini untuk melakukan pencarian refleksi dari vektor
#' berdasarkan pada plane. Jika plane bernilai 'xz', maka
#' akan dilakukan refleksi terhadap bidang xz, jika plane bernilai 'xz',
#' maka akan dilakukan refleksi terhadap bidang xz, sedangkan jika plane bernilai
#' 'yz' maka akan dilakukan refleksi terhadap bidang yz.
#' @examples
#' A = c(3,4,5)
#' plane = "xz"
#' reflection_plane(A, plane)
#' @param A vektor input yang ingin dilakukan transformasi refleksi
#' @param plane bidang refleksi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi refleksi
#' @export

reflection_plane <- function(A, plane){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=3) stop("jumlah data vektor A harus terdiri dari 3 data")

  if (plane == "xy") {
    reflect<- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -1), 3, 3)
  } else if (plane == "xz") {
    reflect <- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 1), 3, 3)
  } else if (plane == "yz") {
    reflect <- matrix(c(-1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  } else {
    stop("Kesalahan input plane, pilih antara 'xy', 'xz', atau 'yz'.")
  }

  cat("Vektor A: \n")
  print(A)

  cat("\nRefleksi matriks : \n")
  print(reflect)

  cat("\nRefleksi Matriks * vektor A : \n")

  reflection = reflect %*% A

  return(reflection)
}
