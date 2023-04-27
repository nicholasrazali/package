
#' Mencari Proyeksi dari vektor R2
#' @description
#' Fungsi ini akan melakukan pencarian proyeksi dari vektor
#' berdasarkan pada axis.
#' @details
#' Fungsi ini untuk melakukan pencarian proyeksi dari vektor
#' berdasarkan pada axis. Jika axis bernilai 'x', maka
#' akan dilakukan proyeksi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan proyeksi terhadap sumbu y.
#' @examples
#' A = c(3,4)
#' axis = "x"
#' projection_axis(A, axis)
#' @param A vektor input yang ingin dilakukan transformasi proyeksi
#' @param axis sumbu proyeksi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi proyeksi
#' @export

projection_axis <- function(A, axis){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=2) stop("jumlah data vektor A harus terdiri dari 2 data")

  if (axis == "x") {
    proyeksi <- matrix(c(1, 0, 0, 0), 2, 2)
  } else if (axis == "y") {
    proyeksi <- matrix(c(0, 0, 0, 1), 2, 2)
  } else {
    stop("Kesalahan input axis, pilih antara 'x' atau 'y'.")
  }

  cat("Vektor A: \n")
  print(A)

  cat("\nProyeksi matriks : \n")
  print(proyeksi)

  cat("\nProyeksi Matriks * vektor A : \n")

  projection = proyeksi %*% A

  return(projection)
}
