
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
#' @return Plot hasil transformasi refleksi
#' @import plotly
#' @export


reflection_vector <- function(A, to) {
  if (!is.vector(A))
    stop("Variabel A harus dalam bentuk vektor.")
  else if (length(A) < 2 || length(A) > 3)
    stop("Jumlah elemen vektor A harus terdiri dari 2 atau 3 elemen.")

  if (length(A) == 2) {
    if (to == "x") {
      reflect <- matrix(c(1, 0, 0, -1), 2, 2)
    } else if (to == "y") {
      reflect <- matrix(c(-1, 0, 0, 1), 2, 2)
    } else if (to == "line") {
      reflect <- matrix(c(0, 1, 1, 0), 2, 2)
    } else {
      stop("Kesalahan input axis, pilih antara 'x', 'y', atau 'line'.")
    }
  } else {
    if (to == "xy") {
      reflect <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -1), 3, 3)
    } else if (to == "xz") {
      reflect <- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 1), 3, 3)
    } else if (to == "yz") {
      reflect <- matrix(c(-1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
    } else {
      stop("Kesalahan input plane, pilih antara 'xy', 'xz', atau 'yz'.")
    }
  }

  reflection <- as.vector(reflect %*% A)

  # Hasil refleksi
  cat("Vektor A:\n")
  print(A)
  cat("\nRefleksi Matriks:\n")
  print(reflect)
  cat("\nHasil Refleksi:\n")
  print(reflection)


  original <- data.frame(x = c(0, A[1]), y = c(0, A[2]), z = c(0, ifelse(length(A) == 3, A[3], 0)))
  reflected <- data.frame(x = c(0, reflection[1]), y = c(0, reflection[2]), z = c(0, ifelse(length(A) == 3, reflection[3], 0)))


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
        x = reflected$x, y = reflected$y,
        line = list(color = "red"),
        name = "Hasil Refleksi"
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
        x = reflected$x, y = reflected$y, z = reflected$z,
        line = list(color = "red"),
        name = "Hasil Refleksi"
      )
  }

  return(p)
}
