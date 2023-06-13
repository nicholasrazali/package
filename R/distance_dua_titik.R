
#' Distance antara 2 Titik
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 2 titik.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 2 titik.
#' pada matriks.
#' @examples
#' point1 <- c(1,2,-2,-3)
#' point2 <- c(2,4,-4,-7)
#' distance_dua_titik(point1, point2)
#' @param point1 vektor dari titik pertama
#' @param point2 vektor dari titik kedua
#' @return Nilai jarak dari 2 titik
#' @export

distance_dua_titik <- function(point1, point2){
  if(is.vector(point1) == FALSE || is.vector(point2) == FALSE) stop("Point harus merupakan vektor")
  else if(length(point1) != length(point2)) stop("Ukuran vektor point harus sama")
  dif = c()

  cat(paste0("dif = point2 - point1 = (",point2[1], "-", point1[1]))
  dif[1] = point2[1] - point1[1]

  for(i in 2:length(point1)){
    cat(paste0(", ", point2[i], "-", point1[i]))
    dif[i] = (point2[i] - point1[i])
  }
  cat(") = (",dif[1])
  for(i in 2:length(dif)){
    cat(paste0(", ", dif[i]))
  }
  cat(")\n")

  dif2 = dif^2
  cat(paste0("\ndif^2 = (",dif2[1]))
  for(i in 2:length(dif2)){
    cat(paste0(", ", dif2[i]))
  }
  cat(")\n")

  cat(paste0("\nJumlah dif^2 = (",dif2[1]))
  for(i in 2:length(dif2)){
    cat(paste0(" + ", dif2[i]))
  }
  cat(") =",sum(dif2),"\n")

  distance = sqrt(sum(dif2))
  cat(paste0("\ndistance = sqrt(",sum(dif2),") = ", distance, "\n"))

  return(distance)
}
