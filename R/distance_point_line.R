
#' Distance antara Titik dan Garis
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 1 titik dan 1 garis,
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 1 titik dan 1 garis DImana titik
#' berada pada 2 dimensi (x,y) dan bidang dengan persamaan ax^2 + bx + c = 0 dituliskan
#' menjadi bentuk vektor (a,b,c)
#' @examples
#' point <- c(-1,2)
#' line <- c(3,-4,9)
#' distance_point_line(point, line)
#' @param point vektor dari titik
#' @param line vektor dari garis
#' @return Nilai jarak dari 1 titik ke 1 garis
#' @export

distance_point_line <- function(point, line){
  if(is.vector(point) == FALSE || is.vector(line) == FALSE) stop("Point dan Plane harus merupakan vektor")
  else if(length(point) != 2) stop("Ukuran vektor point harus terdiri dari 2 buah")
  else if(length(line) != 3) stop("Ukuran vektor line harus terdiri dari 3 buah")


  a <- line[1]
  b <- line[2]
  c <- line[3]

  x <- point[1]
  y <- point[2]

  atas = abs(a*x + b*y + c)
  cat(paste0("atas = |",a, "*",x, " + ",b, "*",y, " + ",c,"|\n" ))
  cat(paste0("     = |",a*x, " + ",b*y, " + ", c, "|\n"))
  cat(paste0("     = |",a*x + b*y + c,"|\n" ))
  cat(paste0("atas = ", atas, "\n"))

  bawah = sqrt(a^2 + b^2)
  cat(paste0("\nbawah = sqrt(",a,"^2 + ",b,"^2",")\n"))
  cat(paste0("      = sqrt(",a^2, " + ",b^2, ")\n"))
  cat(paste0("      = sqrt(",a^2 + b^2,")\n"))
  cat(paste0("bawah = ",bawah,"\n"))

  distance <- atas/bawah
  cat(paste0("\nDistance = ",atas,"/",bawah," = ", distance,"\n"))
  return(distance)
}
