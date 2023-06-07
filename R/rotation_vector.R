
#' Mencari Rotasi dari vektor R2 dan R3
#' @description
#' Fungsi ini akan melakukan pencarian rotasi dari vektor
#' berdasarkan berlawanan arah jarum jam, axis, dan besaran sudut pada dua dimensi atau tiga dimensi.
#' @details
#' Fungsi ini untuk melakukan pencarian rotasi dari vektor
#' berdasarkan berlawanan arah jarum jam, axis, dan besaran sudut pada dua dimensi atau tiga dimensi.
#' Jika berada pada tiga dimensi maka ditambahkan dengan axis atau arah rotasi
#' terhadap sumbu. axis bernilai 'x', maka
#' akan dilakukan rotasi terhadap sumbu x, jika axis bernilai 'y',
#' maka akan dilakukan rotasi terhadap sumbu y, dan jika axis bernilai 'z', maka
#' akan dilakukan rotasi terhadap sumbu 'z'.
#' @examples
#' A = c(3,4)
#' alfa = 60
#' rotation_vector(A, alfa = alfa )
#'
#'
#' A = c(3,4,5)
#' alfa = 60
#' axis = "x"
#' rotation_vector(A, alfa = alfa, axis = axis )
#' @param A vektor input yang ingin dilakukan transformasi rotasi
#' @param alfa besaran sudut
#' @param axis sumbu rotasi terhadap vektor yang akan dilakukan
#' @return Plot hasil transformasi rotasi
#' @import plotly
#' @export

rotation_vector <- function(A, alfa, axis = NULL){
  if (!is.vector(A))
    stop("Variabel A harus dalam bentuk vektor.")
  else if (length(A) < 2 || length(A) > 3)
    stop("Jumlah elemen vektor A harus terdiri dari 2 atau 3 elemen.")

  radian <- alfa * pi / 180

  if(length(A) == 2){
    rotasi <- matrix(c(cos(radian), sin(radian), -sin(radian), cos(radian)), 2, 2)
  }else{
    if (axis == "x") {
      rotasi <- matrix(c(1, 0, 0, 0, cos(radian), sin(radian), 0, -sin(radian), cos(radian)), 3, 3)
    } else if (axis == "y") {
      rotasi <- matrix(c(cos(radian), 0, -sin(radian), 0, 1, 0, sin(radian), 0, cos(radian)), 3, 3)
    } else if (axis == "z") {
      rotasi <- matrix(c(cos(radian), sin(radian), 0, -sin(radian), cos(radian), 0, 0, 0, 1), 3, 3)
    } else {
      stop("Kesalahan input axis, pilih antara 'x', 'y', atau 'z'.")
    }
  }

  rotation = as.vector(rotasi %*% A)

  cat("Vektor A:\n")
  print(A)
  cat("\nRotasi matriks:\n")
  print(rotasi)
  cat("\nHasil Rotasi:\n")
  print(rotation)


  original <- data.frame(x = c(0, A[1]), y = c(0, A[2]), z = c(0, ifelse(length(A) == 3, A[3], 0)))
  rotated <- data.frame(x = c(0, rotation[1]), y = c(0, rotation[2]), z = c(0, ifelse(length(A) == 3, rotation[3], 0)))

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
        x = rotated$x, y = rotated$y,
        line = list(color = "red"),
        name = "Hasil Rotasi"
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
        x = rotated$x, y = rotated$y, z = rotated$z,
        line = list(color = "red"),
        name = "Hasil Rotasi"
      )
  }

  return(p)
}
