
#' Perkalian Matriks dengan Langkah
#' @description
#' Fungsi ini akan melakukan perhitungan perkalian terhadap 2 matriks yang disertai dengan langkah untuk mendapatkan hasilnya.
#'
#' @details
#' Fungsi ini untuk menghitung perkalian dari 2 matrix yang diinput. Dimana dilakukan pengecekan terhadap jumlah baris dan jumlah kolom dari kedua matriks,jika jumlah kolom pada matriks pertama bernilai sama dengan jumlah baris pada matriks kedua maka dapat dilakukan perhitungan perkalian kedua matriks.
#' Untuk langkah yang dihasilkan menunjukkan perkalian setiap elemen baris pada matriks pertama dengan setiap elemen kolom pada matriks kedua, dan juga akan menunjukan hasil perkalian tersebut.
#' @examples
#' x <- matrix(1:4,2,2)
#' y <- matrix(5:8,2,2)
#' perkalian_matriks_dengan_langkah(x,y)
#' @param x matriks input pertama
#' @param y matriks input kedua
#' @return Nilai perkalian dari matriks x dan matriks y
#' @export


perkalian_matriks_dengan_langkah <- function(x, y){
  if(!is.matrix(x) || !is.matrix(y)){
    stop("Input data harus berupa matriks")
  }
  nrow_x = nrow(x)
  ncol_x = ncol(x)

  nrow_y = nrow(y)
  ncol_y = ncol(y)

  if(ncol_x != nrow_y){
    stop("Jumlah kolom pada matriks x dan jumlah baris pada matriks y harus sama")
  }
  else{
    temp = matrix(0,nrow_x,ncol_y)
    temp2 = matrix(0,nrow_x,ncol_y)
    temp3 = matrix(0,nrow_x,ncol_y)
    res = 0
    res2 = 0
    res3 = 0
    count = 0
    count2 = 0
    for (i in 1:nrow_x) {
      for (j in 1:ncol_y) {
        for (k in 1:nrow_y) {
          res = res + x[i,k]*y[k,j]
          res2 = paste(x[i,k],y[k,j],sep = "*")
          count = x[i,k]*y[k,j]
          if(k==1){
            res3 = res2
            count2 = count
          }else {
            res3 = paste(res3 , res2, sep = " + ")
            count2 = paste(count2, count, sep = " + ")
          }
        }
        temp[i,j] = res
        temp2[i,j] = res3
        temp3[i,j] = count2
        count = 0
        count2 = 0
        res2 = 0
        res3 = 0
        res = 0
      }
    }
  }
  return(list("step" = list(temp2,temp3), "hasil" = temp))
}
