
#' Distance antara 2 Titik
#' @description
#' Fungsi ini akan melakukan pencarian nilai jarak antara 2 titik.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai jarak antara 2 titik, dengan melakukan
#' pengurangan terhadap 2 vektor, kemudian hasil pengurangan dipangkatkan 2, dan dijumlahkan
#' untuk hasil dari pangkat tersebut. Terakhir untuk mendapatkan nilai jarak dengan
#' kuadrat dari hasil tersebut. Kemudian akan ditampilkan plot untuk jarak antara
#' titik 1 dengan titik 2.
#' @examples
#' point1 <- c(1,2,-2)
#' point2 <- c(2,4,-4)
#' distance_dua_titik(point1, point2)
#' @param point1 vektor dari titik pertama
#' @param point2 vektor dari titik kedua
#' @return Plot jarak dari 2 titik
#' @export

distance_dua_titik <- function(point1, point2){
  if(is.vector(point1) == FALSE || is.vector(point2) == FALSE) stop("Point harus merupakan vektor")
  else if(length(point1)<1 || length(point1)>3) stop("Vektor yang dimasukkan harus berupa vektor 2 dimensi atau 3 dimensi")
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

  if(length(point1) == 2){
    p <- plot_ly() %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = c(point1[1], point2[1]),
        y = c(point1[2], point2[2])
      )
  }
  else{
    p <- plot_ly() %>%
      add_trace(
        type = "scatter3d",
        mode = "lines",
        x = c(point1[1], point2[1]),
        y = c(point1[2], point2[2]),
        z = c(point1[3], point2[3])
      )
  }



  return(p)
}
