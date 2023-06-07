
#' Mencari Proyeksi dari vektor R2 dan R3
#' @description
#' Fungsi ini akan melakukan pencarian proyeksi dari vektor
#' berdasarkan pada axis atau plane.
#' @details
#' Fungsi ini untuk melakukan pencarian proyeksi dari vektor
#' berdasarkan pada axis atau plane.Jika axis bernilai 'x', maka
#' akan dilakukan proyeksi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan proyeksi terhadap sumbu y. Jika plane bernilai 'xz', maka
#' akan dilakukan proyeksi terhadap bidang xz, jika plane bernilai 'xz',
#' maka akan dilakukan proyeksi terhadap bidang xz, sedangkan jika plane bernilai
#' 'yz' maka akan dilakukan proyeksi terhadap bidang yz.
#' @examples
#' A = c(3,4)
#' to = "x"
#' projection_vector(A, to)
#'
#' A = c(3,4,5)
#' to = "xz"
#' projection_vector(A, to)
#'
#' @param A vektor input yang ingin dilakukan transformasi proyeksi
#' @param to proyeksi terhadap vektor yang akan dilakukan
#' @return Plot hasil transformasi proyeksi
#' @import plotly
#' @export

projection_vector <- function(A, to){
  if (!is.vector(A))
    stop("Variabel A harus dalam bentuk vektor.")
  else if (length(A) < 2 || length(A) > 3)
    stop("Jumlah elemen vektor A harus terdiri dari 2 atau 3 elemen.")

  if (length(A) == 2) {
    if (to == "x") {
      proyeksi <- matrix(c(1, 0, 0, 0), 2, 2)
    } else if (to == "y") {
      proyeksi <- matrix(c(0, 0, 0, 1), 2, 2)
    } else {
      stop("Kesalahan input axis, pilih antara 'x' atau 'y'.")
    }
  } else {
    if (to == "xy") {
      proyeksi<- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0), 3, 3)
    } else if (to == "xz") {
      proyeksi <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 1), 3, 3)
    } else if (to == "yz") {
      proyeksi <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
    } else {
      stop("Kesalahan input plane, pilih antara 'xy', 'xz', atau 'yz'.")
    }
  }

  projection = as.vector(proyeksi %*% A)

  cat("Vektor A:\n")
  print(A)
  cat("\nProyeksi matriks:\n")
  print(proyeksi)
  cat("\nHasil Proyeksi:\n")
  print(projection)

  original <- data.frame(x = c(0, A[1]), y = c(0, A[2]), z = c(0, ifelse(length(A) == 3, A[3], 0)))
  projected <- data.frame(x = c(0, projection[1]), y = c(0, projection[2]), z = c(0, ifelse(length(A) == 3, projection[3], 0)))


  if(length(A) == 2){
    p <- plot_ly() %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = original$x, y = original$y,
        line = list(color = "blue"),
        name = "Vektor Asli"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = projected$x, y = projected$y,
        line = list(color = "red"),
        name = "Hasil Proyeksi"
      )
  }else{
    p <- plot_ly() %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = original$x, y = original$y, z = original$z,
        line = list(color = "blue"),
        name = "Vektor Asli"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = projected$x, y = projected$y, z = projected$z,
        line = list(color = "red"),
        name = "Hasil Proyeksi"
      )
  }

  return(p)
}
