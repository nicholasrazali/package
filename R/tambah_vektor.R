
#' Menghitung Penjumlahan 2 vektor
#' @description
#' Fungsi ini akan melakukan penjumlahan dari 2 vektor dan menghasilkan hasil plot grafik.
#' @details
#' Fungsi ini untuk menghitung penjumlahan dari 2 vektor yang diinput.
#' Dimana dilakukan pengecekan terhadap jumlah elemen dari vektor yang dimasukkan. Kemudian hasil akhir
#' akan ditampilkan juga dalam bentuk plot grafik.
#' @examples
#' u <- c(1,2)
#' v <- c(5,0)
#' tambah_vektor(u,v)
#' @param u vektor input pertama
#' @param v vektor input kedua
#' @return Plot penjumlahan dari vektor u dan vektor v.
#' @export

tambah_vektor <- function(u, v){
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u)<1 || length(u)>3) stop("Vektor yang dimasukkan harus berupa vektor 2 dimensi atau 3 dimensi")
  else if(length(u) != length(v)) stop("Jumlah data vektor yang dimasukkan harus sama")

  cat("Penjumlahan = (")

  for(i in 1:length(u)){
    if(i == length(u)) cat(paste0(u[i], " + ", v[i]))
    else cat(paste0(u[i], " + ", v[i], ", "))
  }
  cat(") = (")

  for(i in 1:length(u)){
    if(i == length(u)) cat(paste0(u[i] + v[i]))
    else cat(paste0(u[i] + v[i], ", "))
  }
  cat(")\n")


  cat("\nVektor Penjumlahan:\n")
  sum = u + v
  print(sum)

  plot_u <- data.frame(x = c(0, u[1]), y = c(0, u[2]), z = c(0, ifelse(length(u) == 3, u[3], 0)))
  plot_v <- data.frame(x = c(0, v[1]), y = c(0, v[2]), z = c(0, ifelse(length(u) == 3, v[3], 0)))
  plot_uv <- data.frame(x = c(0, sum[1]), y = c(0, sum[2]), z = c(0, ifelse(length(u) == 3, sum[3], 0)))


  if(length(u) == 2){
    p <- plot_ly() %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_u$x, y = plot_u$y,
        line = list(color = "blue"),
        name = "Vektor u"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_v$x, y = plot_v$y,
        line = list(color = "red"),
        name = "Vektor v"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_uv$x, y = plot_uv$y,
        line = list(color = "green"),
        name = "Vektor uv"
      )
  }else{
    p <- plot_ly() %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_u$x, y = plot_u$y, z = plot_u$z,
        line = list(color = "blue"),
        name = "Vektor u"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_v$x, y = plot_v$y, z = plot_v$z,
        line = list(color = "red"),
        name = "Vektor u"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_uv$x, y = plot_uv$y, z = plot_uv$z,
        line = list(color = "green"),
        name = "Vektor uv"
      )
  }

  return(p)
}
