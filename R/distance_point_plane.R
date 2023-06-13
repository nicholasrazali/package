
#' Distance antara Titik dan Bidang
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 1 titik dan 1 bidang,
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 1 titik dan 1 bidang. DImana titik
#' berada pada 3 dimensi (x,y,z) dan bidang dengan persamaan ax^3 + bx^2 + cx + d = 0 dituliskan
#' menjadi bentuk vektor (a,b,c,d)
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
  return(distance)
}
