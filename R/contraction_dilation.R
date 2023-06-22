#' Mencari Kontraksi dan Dilatasi dari vektor R2 atau Vektor R3
#' @description
#' Fungsi ini akan melakukan pencarian kontraksi dan dilatasi dari vektor
#' berdasarkan besaran faktor skalar k.
#' @details
#' Fungsi ini untuk melakukan pencarian kontraksi dan dilatasi dari vektor
#' berdasarkan besaran faktor skalar k. Jika besaran k berada di antara 0 dan 1
#' maka dilakukan transformasi kontraksi, sedangkan jika besaran k lebih besar
#' atau sama dengan 1 maka dilakukan transformasi dilatasi. Jika data yang dimasukan berjumlah 2 maka
#' dilakukan untuk vektor R2, jika berjumlah 3 maka dilakukan vektor R3.
#' @examples
#' A = c(3,4)
#' k = 3
#' contraction_dilatation(A, k)
#' @param A vektor input yang ingin dilakukan transformasi kontraksi dan dilatasi
#' @param k besaran skalar k
#' @return Plot hasil transformasi kontraksi dan dilatasi
#' @export

contraction_dilatation <- function(A, k){
  if(is.vector(A) == FALSE) stop("Variabel A harus dalam vektor")
  else if(length(A)<2 || length(A)>3) stop("jumlah data vektor A harus terdiri dari 2 atau 3 data")
  else if(is.numeric(k) == FALSE || k < 0) stop("Variabel k harus dalam angka dan harus lebih besar atau sama dengan 0")

  transformasi = diag(length(A)) * k

  transformation = as.vector(transformasi %*% A)

  cat("Vektor A:\n")
  print(A)
  cat("\nTransformasi matriks:\n")
  print(transformasi)
  cat("\nHasil transformasi:\n")
  print(transformation)



  original <- data.frame(x = c(0, A[1]), y = c(0, A[2]), z = c(0, ifelse(length(A) == 3, A[3], 0)))
  transformed <- data.frame(x = c(0, transformation[1]), y = c(0, transformation[2]), z = c(0, ifelse(length(A) == 3, transformation[3], 0)))


  width_ori = ifelse(k > 1, 3, 1.5)
  width_trans = ifelse(k > 1, 1.5, 3)

  if(length(A) == 2){
    p <- plot_ly() %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = original$x, y = original$y,
        line = list(color = "blue",width = width_ori),
        name = "Vektor Asli"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = transformed$x, y = transformed$y,
        line = list(color = "red",width = width_trans),
        name = "Hasil Transformasi"
      ) %>%
      layout(
        xaxis = list(
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          scaleanchor = "x",
          scaleratio = 1
        )
      )
  }else{
    p <- plot_ly() %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = original$x, y = original$y, z = original$z,
        line = list(color = "blue",width = width_ori),
        name = "Vektor Asli"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = transformed$x, y = transformed$y, z = transformed$z,
        line = list(color = "red",width = width_trans),
        name = "Hasil Transformasi"
      ) %>%
      layout(
        scene = list(
          aspectmode = "data"
        )
      )


  }

  return(p)
}
