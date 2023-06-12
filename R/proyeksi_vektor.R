
#' Proyeksi Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai proyeksi skalar dan proyeksi ortogonal.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian pencarian nilai proyeksi skalar dan proyeksi ortogonal,
#' dimana untuk dua vektor yang tidak berhimpit sehingga salah satu vektor bisa
#' diproyeksikan ke vektor lainnya.
#' @examples
#' a <- c(1,3)
#' b <- c(6,2)
#' proyeksi_vektor(a,b)
#' @param a vektor input pertama
#' @param b vektor input pertama
#' @return Plot vektor proyeksi dan proyeksi ortogonal.
#' @export

proyeksi_vektor <- function(a, b){
  if(!is.vector(a) || !is.vector(b))stop("Input data harus berupa vektor")
  else if(length(a) != length(b)) stop("Jumlah data vektor yang dimasukkan harus sama")

  cat(paste0("ab = ",a[1],"*",b[1]))

  for(i in 2:length(a)){
    cat(paste0(" + ",a[i],"*",b[i]))
  }
  cat(" =", sum(a*b), "\n")

  ab = sum(a*b)
  norm_b2 =sum(b^2)
  norm_b = sqrt(norm_b2)
  cat(paste0("norm b^2 = ",norm_b2),"\n")
  cat(paste0("norm b = ",norm_b),"\n")

  skalar = ab/norm_b
  cat(paste0("\nskalar = ",ab,"/",norm_b," = ", skalar, "\n"))

  proyeksi = ab/norm_b2  * b
  cat(paste0("\nproyeksi = (",ab,"/",norm_b, ") * (",b[1]))
  for(i in 2:length(b)){
    cat(paste0(", ",b[i]))
  }
  cat(") = (", proyeksi[1])
  for(i in 2:length(proyeksi)){
    cat(paste0(", ",proyeksi[i]))
  }
  cat(")","\n\n")

  ortogonal = a - proyeksi
  cat(paste0("\nOrtogonal = ("))

  for(i in 1:length(a)){
    if(i == length(a)) cat(paste0(a[i], " - ", proyeksi[i]))
    else cat(paste0(a[i], " - ", proyeksi[i], ", "))
  }
  cat(") = (", ortogonal[1])
  for(i in 2:length(ortogonal)){
    cat(paste0(", ",ortogonal[i]))
  }
  cat(")","\n\n")

  skalar = round(skalar,5)
  proyeksi = round(proyeksi,5)
  ortogonal = round(ortogonal,5)


  cat("Panjang vektor proyeksi = ", skalar, "\n")

  cat(paste0("Proyeksi vektor  = (", proyeksi[1]))
  for(i in 2:length(proyeksi)){
    cat(paste0(", ",proyeksi[i]))
  }
  cat(")","\n")

  cat("Proyeksi vektor ortogonal = (", ortogonal[1])
  for(i in 2:length(ortogonal)){
    cat(paste0(", ",ortogonal[i]))
  }
  cat(")","\n")


  plot_a <- data.frame(x = c(0, a[1]), y = c(0, a[2]), z = c(0, ifelse(length(a) == 3, a[3], 0)))
  plot_b <- data.frame(x = c(0, b[1]), y = c(0, b[2]), z = c(0, ifelse(length(b) == 3, b[3], 0)))
  plot_proy <- data.frame(x = c(0, proyeksi[1]), y = c(0, proyeksi[2]), z = c(0, ifelse(length(a) == 3, proyeksi[3], 0)))
  plot_orto <- data.frame(x = c(0, ortogonal[1]), y = c(0, ortogonal[2]), z = c(0, ifelse(length(a) == 3, ortogonal[3], 0)))

  if(length(a) == 2){
    p <- plot_ly() %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_a$x, y = plot_a$y,
        line = list(color = "blue"),
        name = "Vektor a"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_b$x, y = plot_b$y,
        line = list(color = "red"),
        name = "Vektor b"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_proy$x, y = plot_proy$y,
        line = list(color = "green"),
        name = "Vektor proyeksi"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = plot_orto$x, y = plot_orto$y,
        line = list(color = "black"),
        name = "Vektor ortogonal"
      )
  }else{
    p <- plot_ly() %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_a$x, y = plot_a$y, z = plot_a$z,
        line = list(color = "blue"),
        name = "Vektor a"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_b$x, y = plot_b$y, z = plot_b$z,
        line = list(color = "red"),
        name = "Vektor b"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_proy$x, y = plot_proy$y, z = plot_proy$z,
        line = list(color = "green"),
        name = "Vektor proyeksi"
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_orto$x, y = plot_orto$y, z = plot_orto$z,
        line = list(color = "black"),
        name = "Vektor ortogonal"
      )

  }


  return(p)
}
