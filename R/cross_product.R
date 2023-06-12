
#' Cross Product Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai dot product vektor.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai dot product vektor.
#' pada matriks.
#' @examples
#' u <- c(0,0,1)
#' v <- c(0,2,2)
#' cross_product(u,v)
#' @param u vektor input pertama
#' @param v vektor input pertama
#' @return Nilai cross product dari 2 vektor
#' @export

cross_product <- function(u, v) {
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u) != 3 || length(v) != 3) stop("Vektor harus memiliki sejumlah 3 data")
  cat(paste0("cross_product = ((",u[2], "*",v[3] ," - ", u[3], "*",v[2],"),(",u[3], "*",v[1] ," - ", u[1], "*",v[3],"),(",u[1], "*",v[2] ," - ", u[2], "*",v[1],"))\n"))
  cat(paste0(" = ((",u[2]*v[3], " - ", u[3]*v[2],"), (",u[3]*v[1]," - ",u[1]*v[3],"), (",u[1]*v[2]," - ",u[2]*v[1],"))\n" ))
  cat(paste0(" = (",u[2]*v[3] - u[3]*v[2],", ",u[3]*v[1] - u[1]*v[3], ", ",u[1]*v[2] - u[2]*v[1],")\n"))

  cross = c(u[2]*v[3] - u[3]*v[2], u[3]*v[1] - u[1]*v[3], u[1]*v[2] - u[2]*v[1])

  plot_u <- data.frame(x = c(0, u[1]), y = c(0, u[2]), z = c(0, u[3]))
  plot_v <- data.frame(x = c(0, v[1]), y = c(0, v[2]), z = c(0, v[3]))
  plot_cross <- data.frame(x = c(0, cross[1]), y = c(0, cross[2]), z = c(0, cross[3]))

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
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = plot_cross$x, y = plot_cross$y, z = plot_cross$z,
        line = list(color = "green"),
        name = "Vektor cross"
      )

  return(p)
}
