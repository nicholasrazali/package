
#' Mencari Proyeksi dari vektor R3
#' @description
#' Fungsi ini akan melakukan pencarian proyeksi dari vektor
#' berdasarkan pada plane.
#' @details
#' Fungsi ini untuk melakukan pencarian proyeksi dari vektor
#' berdasarkan pada plane. Jika plane bernilai 'xz', maka
#' akan dilakukan proyeksi terhadap bidang xz, jika plane bernilai 'xz',
#' maka akan dilakukan proyeksi terhadap bidang xz, sedangkan jika plane bernilai
#' 'yz' maka akan dilakukan proyeksi terhadap bidang yz.
#' @examples
#' A = c(3,4,5)
#' plane = "xz"
#' projection_plane(A, plane)
#' @param A vektor input yang ingin dilakukan transformasi proyeksi
#' @param plane bidang proyeksi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi proyeksi
#' @export

projection_plane <- function(A, plane){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=3) stop("jumlah data vektor A harus terdiri dari 3 data")

  if (plane == "xy") {
    proyeksi<- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)
  } else if (plane == "xz") {
    proyeksi <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 1), 3, 3)
  } else if (plane == "yz") {
    proyeksi <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
  } else {
    stop("Kesalahan input plane, pilih antara 'xy', 'xz', atau 'yz'.")
  }

  cat("Vektor A: \n")
  print(A)

  cat("\nProyeksi matriks : \n")
  print(proyeksi)

  cat("\nProyeksi Matriks * vektor A : \n")

  projection = proyeksi %*% A

  return(projection)
}
