
#' Menghitung Operasi Dasar Matriks
#' @description
#' Fungsi ini akan melakukan perhitungan operasi dasar dari 2 matriks
#' berupa penjumlahan, pengurangan, dan perkalian matriks jika memenuhi syarat seperti jumlah baris dan jumlah kolom yang sesuai.
#'
#' @details
#' Fungsi ini untuk menghitung penjumlahan, pengurangan dan perkalian dari 2 matrix yang diinput.
#' Dimana dilakukan pengecekan terhadap jumlah baris dan jumlah kolom dari kedua matriks,
#' jika memiliki jumlah baris dan jumlah kolom yang sama maka dapat dilakukan perhitungan
#' penjumlahan dan pengurangan dari kedua matriks. Jika jumlah kolom pada matriks pertama
#' bernilai sama dengan jumlah baris pada matriks kedua maka dapat dilakukan perhitungan
#' perkalian kedua matriks.
#' @examples
#' x <- matrix(1:4,2,2)
#' y <- matrix(5:8,2,2)
#' hitung_matriks(x,y)
#' @param x matriks input pertama
#' @param y matriks input kedua
#' @return Nilai penjumlahan, pengurangan, dan perkalian dari matriks x dan matriks y
#' @export

hitung_matriks <- function(x, y){
  if(!is.matrix(x) || !is.matrix(y)){
    stop("Input data harus berupa matriks")
  }
  nrow_x = nrow(x)
  ncol_x = ncol(x)

  nrow_y = nrow(y)
  ncol_y = ncol(y)

  check  = dim(x) == dim(y)

  if(check[1] & check[2]){
    sum = x+y
    min = x-y
  }
  else{
    sum = "Tidak bisa melakukan penjumlahan karena perbedaan dimensi"
    min = "Tidak bisa melakukan pengurangan karena perbedaan dimensi"
  }

  if(ncol_x == nrow_y){
    multiply = x %*% y
  }
  else multiply = "Tidak bisa melakukan perkalian karena jumlah kolom matrix x != jumlah baris matrix y"

  return(list("Penjumlahan" = sum, "Pengurangan" = min, "Perkalian" = multiply))
}
