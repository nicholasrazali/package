
#' Mencari Rotasi dari vektor R3
#' @description
#' Fungsi ini akan melakukan pencarian rotasi dari vektor
#' berdasarkan berlawanan arah jarum jam, axis, dan besaran sudut pada tiga dimensi.
#' @details
#' Fungsi ini untuk melakukan pencarian rotasi dari vektor
#' berdasarkan berlawanan arah jarum jam, axis, dan besaran sudut pada tiga dimensi. Jika axis bernilai 'x', maka
#' akan dilakukan rotasi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan rotasi terhadap sumbu y, dan jika axis bernilai 'z', maka
#' akan dilakukan rotasi terhadap sumbu 'z'.
#' @examples
#' A = c(3,4,5)
#' alfa = 60
#' axis = "x"
#' rotation_plane(A, alfa = alfa, axis = axis )
#' @param A vektor input yang ingin dilakukan transformasi rotasi
#' @param alfa besaran sudut
#' @param axis sumbu rotasi terhadap vektor yang akan dilakukan
#' @return Hasil transformasi rotasi
#' @export

rotation_plane <- function(A, alfa, axis){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)!=3) stop("jumlah data vektor A harus terdiri dari 3 data")
  else if(is.numeric(alfa) == FALSE) stop("Variabel alfa harus dalam angka")

  radian <- alfa * pi / 180
  rotasi <- matrix(c(cos(radian), sin(radian), -sin(radian), cos(radian)), 2, 2)

  if (axis == "x") {
    rotasi <- matrix(c(1, 0, 0, 0, cos(radian), sin(radian), 0, -sin(radian), cos(radian)), 3, 3)
  } else if (axis == "y") {
    rotasi <- matrix(c(cos(radian), 0, -sin(radian), 0, 1, 0, sin(radian), 0, cos(radian)), 3, 3)
  } else if (axis == "z") {
    rotasi <- matrix(c(cos(radian), sin(radian), 0, -sin(radian), cos(radian), 0, 0, 0, 1), 3, 3)
  } else {
    stop("Kesalahan input axis, pilih antara 'x', 'y', atau 'z'.")
  }

  cat("Vektor A: \n")
  print(A)

  cat("\nRotasi matriks : \n")
  print(rotasi)

  cat("\nRotasi Matriks * vektor A : \n")

  rotation = rotasi %*% A

  return(rotation)
}
