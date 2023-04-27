
#' Distance antara Bidang Paralel
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 2 bidang paralel.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian pencarian nilai jarak antara 2 bidang paralel. by_first_plane digunakan
#' untuk menentukan bidang mana yang ingin dijadikan sebagai tujuannya. Jika bernilai TRUE
#' maka bidang pertama sebagai tujuan, sedangkan jika bernilai FALSE maka bidang
#' kedua akan dijadikan sebagai tujuan. Untuk vektor pada masing-masing bidang
#' harus memiliki ukuran yang sama.
#' @examples
#' plane1 <- c(1,2,-2,-3)
#' plane2 <- c(2,4,-4,-7)
#' distance_parallel_plane(plane1, plane2)
#' @param plane1 vektor dari bidang pertama
#' @param plane2 vektor dari bidang kedua
#' @param by_first_plane penentuan bidang yang akan dijadikan nilai y dan z nya bernilai 0
#' @return Nilai jarak dari 2 bidang paralel
#' @export

distance_parallel_plane <- function(plane1, plane2, by_first_plane = TRUE){
  if(is.vector(plane1) == FALSE || is.vector(plane1) == FALSE) stop("Plane harus merupakan vektor")
  else if(length(plane1) != length(plane2)) stop("Ukuran vektor antara kedua plane harus sama")


  if(by_first_plane == TRUE){
    a <- plane2[1]
    b <- plane2[2]
    c <- plane2[3]
    d <- plane2[4]

    x <- -plane1[4]/plane1[1]
    y <- 0
    z <- 0

    cat("Karena by_first_plane = TRUE, maka plane 1 dijadikan sebagai point\n")
    cat(paste0("x = -(",plane1[4],")/",plane1[1], " = ", x , "\n"))
    cat("y = z = 0\n")
    cat(paste0("point= (",x,", 0, 0)\n"))
  }else{
    a <- plane1[1]
    b <- plane1[2]
    c <- plane1[3]
    d <- plane1[4]

    x <- -plane2[4]/plane2[1]
    y <- 0
    z <- 0
    cat("Karena by_first_plane = FALSE, maka plane 2 dijadikan sebagai point\n")
    cat(paste0("x = -(",plane2[4],")/",plane2[1], " = ", x , "\n"))
    cat("y = z = 0\n")
    cat(paste0("point= (",x,", 0, 0)\n"))
  }

  atas = abs(a*x + b*y + c*z + d)
  cat(paste0("\natas = |",a, "*",x, " + ",b, "*",y, " + ",c,"*",z, " + ",d, "|\n" ))
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
