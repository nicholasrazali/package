
#' Distance antara Titik dan Bidang
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 1 titik dan 1 bidang,
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 1 titik dan 1 bidang. DImana titik
#' berada pada 3 dimensi (x,y,z) dan bidang dengan persamaan ax + by + cz + d = 0 dituliskan
#' menjadi bentuk vektor (a,b,c,d). Kemudian akan ditampilkan plot untuk jarak antara
#' titik dengan bidang.
#'
#' #' Contoh jika ingin mencari jarak pada titik (1,-4,-3) dan persamaan bidnag
#' 2x - 3y + 6z + 1 = 0, maka untuk bidang dituliskan menjadi
#' (2,-3,6,1), maka untuk menggunakan fungsi ini dengan:
#'
#' point <- c(1,-4,-3)
#'
#' plane <- c(2,-3,6,1)
#'
#' distance_point_plane(point,plane)
#' @examples
#' point <- c(1,-4,-3)
#' plane <- c(2,-3,6,1)
#' distance_point_plane(point,plane)
#' @param point vektor dari titik
#' @param plane vektor dari bidang
#' @return Plot jarak dari 1 titik ke 1 bidang
#' @export

distance_point_plane <- function(point, plane){
  if(is.vector(point) == FALSE || is.vector(plane) == FALSE) stop("Point dan Plane harus merupakan vektor")
  else if(length(point) != 3) stop("Ukuran vektor point harus terdiri dari 3 buah")
  else if(length(plane) != 4) stop("Ukuran vektor plane harus terdiri dari 4 buah")
  a <- plane[1]
  b <- plane[2]
  c <- plane[3]
  d <- plane[4]

  x <- point[1]
  y <- point[2]
  z <- point[3]

  cat("Distance = |a*x0 + b*y0 + c*z0 + d|/sqrt(a^2 + b^2 + c^2)\n\n")
  cat("a =",a,"\n")
  cat("b =",b,"\n")
  cat("c =",c,"\n")
  cat("d =",d,"\n")
  cat("x0 =",x,"\n")
  cat("y0 =",y,"\n")
  cat("z0 =",z,"\n\n")

  atas = abs(a*x + b*y + c*z + d)
  cat(paste0("|a*x0 + b*y0 + c*z0 + d |= |",a, "*",x, " + ",b, "*",y, " + ",c,"*",z, " + ",d, "|\n" ))
  cat(paste0("                         = |",a*x, " + ",b*y, " + ", c*z, " + ", d, "|\n"))
  cat(paste0("                         = |",a*x + b*y + c*z + d,"|\n" ))
  cat(paste0("|a*x0 + b*y0 + c*z0 + d |= ", atas, "\n\n"))

  bawah = sqrt(a^2 + b^2 + c^2)
  cat(paste0("sqrt(a^2 + b^2 + c^2) = sqrt(",a,"^2 + ",b,"^2 + ",c,"^2",")\n"))
  cat(paste0("                        = sqrt(",a^2, " + ",b^2, " + ", c^2, ")\n"))
  cat(paste0("                        = sqrt(",a^2 + b^2 + c^2,")\n"))
  cat(paste0("sqrt(a^2 + b^2 + c^2) = ",bawah,"\n"))

  distance <- atas/bawah
  cat(paste0("\nDistance = ",atas,"/",bawah," = ", distance,"\n"))

  x_plane <- seq((x-5), (x+5), by = 0.5)
  y_plane <- seq((y-5), (y+5), by = 0.5)
  z_plane <- outer(x_plane, y_plane, FUN = function(x, y) (-a * x - b * y - d) / c)

  p <- plot_ly() %>%
    add_surface(x = x_plane, y = y_plane, z = z_plane) %>%
    add_markers(x = x, y = y, z = z, color = I("red"), size = 5) %>%
    layout(scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Z")
    ),
    annotations = list(
      x = x, y = y, z = z,
      text = paste("Distance:", distance),
      showarrow = FALSE,
      font = list(color = "red")
    ))

  return(p)
}
