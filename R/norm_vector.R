
#' Norm atau panjang dari vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai panjang dari suatu vektor
#' @details
#' Fungsi ini untuk melakukan pencarian nilai panjang dari suatu vektor dengan
#' menjumlahkan nilai dari vektor yang sudah di pangkatkan 2, kemudian nilai dari
#' penjumlahan tersebut akan dikuadratkan.
#' @examples
#' u <- c(1,2,1)
#' norm_vector(u)
#' @param u variabel vektor
#' @return Nilai panjang dari vektor
#' @export

norm_vector <- function(u){
  if(is.vector(u) == FALSE)stop("Variabel input harus dalam bentuk vektor")
  cat("Vektor awal u : \n")
  print(u)
  cat("\nHasil vektor dipangkatkan 2\n")
  u2 = u^2
  print(u2)

  cat("\nJumlah =",u2[1] )
  for(i in 2:length(u)){
    cat(" +", u2[i])
  }
  cat(" =", sum(u2), "\n")
  norm = sqrt(sum(u2))
  cat(paste0("\nPanjang Vektor = sqrt(", sum(u2),") = ",norm, "\n"))
  return(norm)
}
