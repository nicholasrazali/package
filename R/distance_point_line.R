
#' Distance antara Titik dan Garis
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 1 titik dan 1 garis,
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 1 titik dan 1 garis DImana titik
#' berada pada 2 dimensi (x,y) dan garis dengan persamaan ax + by + c = 0 dituliskan
#' menjadi bentuk vektor (a,b,c). Kemudian akan ditampilkan plot untuk jarak antara
#' titik dengan garis.
#'
#' Contoh jika ingin mencari jarak pada titik (1,2) dan persamaan garis 2x + y - 3 = 0, maka untuk garis dituliskan menjadi
#' (2,1,-3), maka untuk menggunakan fungsi ini dengan:
#'
#' point <- c(1,2)
#'
#' line <- c(2,1,-3)
#'
#' distance_point_line(point,line)
#' @examples
#' point <- c(1,2)
#' line <- c(2,1,-3)
#' distance_point_line(point, line)
#' @param point vektor dari titik
#' @param line vektor dari garis
#' @return Plot jarak dari 1 titik ke 1 garis
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

  cat("Distance = |a*x0 + b*y0 + c|/sqrt(a^2 + b^2)\n\n")
  cat("a =",a,"\n")
  cat("b =",b,"\n")
  cat("c =",c,"\n")
  cat("x0 =",x,"\n")
  cat("y0 =",y,"\n\n")

  atas = abs(a*x + b*y + c)
  cat(paste0("|a*x0 + b*y0 + c| = |",a, "*",x, " + ",b, "*",y, " + ",c,"|\n" ))
  cat(paste0("                  = |",a*x, " + ",b*y, " + ", c, "|\n"))
  cat(paste0("                  = |",a*x + b*y + c,"|\n" ))
  cat(paste0("|a*x0 + b*y0 + c| = ", atas, "\n\n"))

  bawah = sqrt(a^2 + b^2)
  cat(paste0("sqrt(a^2 + b^2) = sqrt(",a,"^2 + ",b,"^2",")\n"))
  cat(paste0("                = sqrt(",a^2, " + ",b^2, ")\n"))
  cat(paste0("                = sqrt(",a^2 + b^2,")\n"))
  cat(paste0("sqrt(a^2 + b^2) = ",bawah,"\n"))

  distance <- atas/bawah
  cat(paste0("\nDistance = ",atas,"/",bawah," = ", distance,"\n"))


  x_line <- seq((x-5), (x+5), length.out = 100)
  y_line <- (-c - a * x_line) / b

  p <- plot_ly() %>%
    add_trace(type = "scatter", mode = "lines", x = x_line, y = y_line, name = "Line") %>%
    add_markers(x = x, y = y, color = I("red"), size = 5, name = "Point") %>%
    layout(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      annotations = list(
        x = x, y = y,
        text = paste("Distance:", distance),
        showarrow = FALSE,
        font = list(color = "red")
      )
    )

  return(p)
}
