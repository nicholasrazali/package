
#' Mencari Refleksi dari vektor R2 dan R3
#' @description
#' Fungsi ini akan melakukan pencarian refleksi dari vektor
#' berdasarkan pada axis, garis, ataupun plane.
#' @details
#' Fungsi ini untuk melakukan pencarian refleksi dari vektor
#' berdasarkan pada axis, garis, ataupun plane. Jika axis bernilai 'x', maka
#' akan dilakukan refleksi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan refleksi terhadap sumbu y, sedangkan jika axis bernilai
#' 'line' maka akan dilakukan refleksi terhadap line y = x. Jika plane bernilai 'xz', maka
#' akan dilakukan refleksi terhadap bidang xz, jika plane bernilai 'xz',
#' maka akan dilakukan refleksi terhadap bidang xz, sedangkan jika plane bernilai
#' 'yz' maka akan dilakukan refleksi terhadap bidang yz.
#' @examples
#' A = c(3,4)
#' to = "x"
#' reflection_vector(A, to)
#'
#' A = c(3,4,5)
#' to = "xz"
#' reflection_vector(A, to)
#' @param A vektor input yang ingin dilakukan transformasi refleksi
#' @param to refleksi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi refleksi
#' @export

reflection_vector <- function(A, to){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A) < 2 || length(A) > 3) stop("jumlah data vektor A harus terdiri dari 2 atau 3 data")

  if(length(A) == 2){
      if (to == "x") {
        reflect<- matrix(c(1, 0, 0, -1), 2, 2)
      } else if (to == "y") {
        reflect <- matrix(c(-1, 0, 0, 1), 2, 2)
      } else if (to == "line") {
        reflect <- matrix(c(0, 1, 1, 0), 2, 2)
      } else {
        stop("Kesalahan input axis, pilih antara 'x', 'y', atau 'line'.")
      }
  } else{
    if (to == "xy") {
      reflect<- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -1), 3, 3)
    } else if (to == "xz") {
      reflect <- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 1), 3, 3)
    } else if (to == "yz") {
      reflect <- matrix(c(-1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
    } else {
      stop("Kesalahan input plane, pilih antara 'xy', 'xz', atau 'yz'.")
    }
  }


  cat("Vektor A: \n")
  print(A)

  cat("\nRefleksi matriks : \n")
  print(reflect)

  cat("\nRefleksi Matriks * vektor A : \n")

  reflection = as.vector(reflect %*% A)

  return(reflection)
}
