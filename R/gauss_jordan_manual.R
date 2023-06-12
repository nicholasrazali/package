#' Eliminasi Gauss-Jordan Secara Manual
#' @description
#' Fungsi ini akan melakukan elimanasi Gauss-Jordan untuk menyelesaikan sistem persamaan
#' linear secara manual dimana untuk prosesnya pengerjaannya ditentukan sendiri.
#'
#' @details
#' Fungsi ini untuk melakukan penyelesaian sistem persamaan linear dengan menggunakan eliminasi Gauss-Jordan.
#' Proses eliminasi Gauss-Jordan dilakukan sesuai dengan input fungsi yang diberikan, terdiri dari tukar() untuk
#' menukar 2 baris, kali() untuk mengalikan baris tertentu dengan nilai yang diberikan, lipat() untuk
#' menjumlahkan barsi, serta hasil() untuk melihat hasil akhir yang diberikan.

#' @examples
#' A <- matrix(c(1,1,0,0,-1,2,1,0,1),3,3)
#' b <- c(4,-1,7)
#' tes = gauss_jordan_manual(A,b)
#' tes$lipat(1,2,-1)
#' tes$kali(2,-1)
#' tes$lipat(2,3,-2)
#' tes$kali(3,-1)
#' tes$lipat(3,2,-1)
#' tes$lipat(3,1,-1)
#' tes$hasil()
#' @param A variabel matriks persegi
#' @param b variabel vektor
#' @return Penyelesaian persamaan linear
#' @export

gauss_jordan_manual <- function(A,b){
  if(is.matrix(A) == FALSE) stop("variabel A harus dalam matriks")
  else if(is.vector(b) == FALSE) stop("variabel b harus dalam vector")

  n <- nrow(A)
  m <- ncol(A)

  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")
  else if(n != length(b)) stop("jumlah vector b tidak sama dengan jumlah baris dan kolom pada matriks")

  cat("Matriks awal\n")
  print(A)

  x   <<- cbind(A, b)

  cat("\nStep 1: Menjadikan Augmented Matriks\n")
  print(x)

  list(
    x.asal <- x,
    tukar = function(i,j){
      if(i > n || j > n) stop("baris yang ingin ditukar tidak bisa lebih besar dari ukuran matriks")
      temp <- x
      temp[c(i, j), ] <- temp[c(j, i), ]
      x <<- temp
      return(x)

    },
    kali = function(i,k){
      if(i > n) stop("baris yang ingin dikalikan tidak bisa lebih besar dari ukuran matriks")
      ind <- diag(nrow(x))
      ind[i,] <- k*ind[i,]
      x <<- ind %*% x
      return(x)
    },
    lipat = function(i, j, k){
      if(i > n || j > n) stop("baris yang ingin ditukar tidak bisa lebih besar dari ukuran matriks")
      ind <- diag(nrow(x))
      ind[j,i] <- k
      x <<- ind %*% x
      return(x)
    },
    hasil = function(){
      return(x)
    }
  )
}
