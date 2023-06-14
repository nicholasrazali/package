
#' Dot Product Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai dot product vektor.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai dot product vektor, kemudian menghitung
#' nilai dari sudut antara 2 vektor dan akan menampilkan plot grafik dari 2 vektor yang dimasukkan.
#' @examples
#' u <- c(3,0)
#' v <- c(4,4)
#' dot_product(u,v)
#' @param u vektor input pertama
#' @param v vektor input pertama
#' @return Plot dari 2 vektor
#' @export

dot_product <- function(u, v){
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u) != length(v)) stop("Jumlah data vektor yang dimasukkan harus sama")

  u1 = u^2
  v1 = v^2
  norm_u = sqrt(sum(u1))
  norm_v = sqrt(sum(v1))

    cat(paste0("dot_product = ",u[1],"*",v[1]))
    for(i in 2:length(u)){
      cat(paste0(" + ",u[i],"*",v[i]))
    }
    cat(" =", sum(u*v), "\n")
    dot = sum(u*v)

  cos_teta = dot/(norm_u*norm_v)
  alfa = acos(cos_teta)*180/pi

  cat(paste0("\nnorm_u = ", norm_u, "\n"))
  cat(paste0("norm_v = ", norm_v, "\n"))
  cat(paste0("\ncos teta = dot / (norm_u * norm_v)\n"))
  cat(paste0("cos teta = ",dot," / (", norm_u, " * ", norm_v, ")\n"))
  cat(paste0("cos teta = ",cos_teta, "\n"))
  cat(paste0("\nteta = acos(cos teta) = acos(",cos_teta,") = ",alfa))

  plot_u <- data.frame(x = c(0, u[1]), y = c(0, u[2]), z = c(0, ifelse(length(u) == 3, u[3], 0)))
  plot_v <- data.frame(x = c(0, v[1]), y = c(0, v[2]), z = c(0, ifelse(length(v) == 3, v[3], 0)))

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
      )
  } else {
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
        name = "Vektor v"
      )
  }


  return(p)
}
