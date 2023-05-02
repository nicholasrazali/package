
#' Distance antara Titik dan Bidang
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 1 titik dan 1 bidang,
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 1 titik dan 1 bidang.
#' @examples
#' point <- c(1,-4,-3)
#' plane <- c(2,-3,6,1)
#' distance_point_plane(point,plane)
#' @param point vektor dari titik
#' @param plane vektor dari bidang
#' @return Nilai jarak dari 1 titik ke 1 bidang
#' @export

distance_point_plane <- function(point, plane){
  if(is.vector(point) == FALSE || is.vector(plane) == FALSE) stop("Point dan Plane harus merupakan vektor")
  else if(length(point) != length(plane)-1) stop("Ukuran vektor point harus merupakan ukuran vektor plane dikurangi 1")

  a <- plane[1]
  b <- plane[2]
  c <- plane[3]
  d <- plane[4]

  x <- point[1]
  y <- point[2]
  z <- point[3]

  atas = abs(a*x + b*y + c*z + d)
  cat(paste0("atas = |",a, "*",x, " + ",b, "*",y, " + ",c,"*",z, " + ",d, "|\n" ))
  cat(paste0("     = |",a*x, " + ",b*y, " + ", c*z, " + ", d, "|\n"))
  cat(paste0("     = |",a*x + b*y + c*z + d,"|\n" ))
  cat(paste0("atas = ", atas, "\n"))

  bawah = sqrt(a^2 + b^2 + c^2)
  cat(paste0("\nbawah = sqrt(",a,"^2 + ",b,"^2 + ",c,"^2",")\n"))
  cat(paste0("      = sqrt(",a^2, " + ",b^2, " + ", c^2, ")\n"))
  cat(paste0("      = sqrt(",a^2 + b^2 + c^2,")\n"))
  cat(paste0("bawah = ",bawah,"\n"))

  distance <- atas/bawah
  cat(paste0("\nDistance = ",atas,"/",bawah," = ", distance,"\n"))
  return(distance)
}