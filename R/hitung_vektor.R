
#' Menghitung Operasi Dasar Vektor
#' @description
#' Fungsi ini akan melakukan perhitungan operasi dasar dari 2 vektor berupa penjumlahan, pengurangan, dan perkalian.
#' @details
#' Fungsi ini untuk menghitung penjumlahan, pengurangan dan perkalian dari 2 vektor yang diinput. Dimana dilakukan pengecekan terhadap jumlah data dari vektor yang dimasukkan.
#' @examples
#' u <- c(1:4)
#' v <- c(5:8)
#' hitung_vektor(u,v)
#' @param u vektor input pertama
#' @param v vektor input kedua
#' @return Nilai penjumlahan, pengurangan, dan perkalian dari vektor u dan vektor v.
#' @export

hitung_vektor <- function(u, v){
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u) != length(v)) stop("Jumlah data vektor yang dimasukkan harus sama")

  sum = u + v
  min = v - u
  multiply = u * v

  return(list("Penjumlahan" = sum, "Pengurangan" = min, "Perkalian" = multiply))
}
