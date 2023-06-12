
#' Invers dengan adjoint Matriks
#' @description
#' Fungsi ini akan melakukan pencarian nilai invers dengan menggunakan adjoint matriks.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai invers dengan menggunakan adjoint matriks, yaitu dengan membuat matriks kofaktor sesuai
#' dengan nilai dari kofaktor untuk baris dan kolom matriks tersebut kemudian dikalian dengan
#' (-1) pangkat penjumlahan dari baris dan kolom tesebut. Setelah mendapatkan
#' matriks kofaktor, selanjutnya melakukan transpose dari matriks kofaktor untuk mendapatkan
#' adjoint matriks. Setelah itu melakukan perkalian dari 1/determinan matriks dengan adjoint matriks.
#' @examples
#' x <- matrix(c(0,3,2,1,-6,6,5,9,1),3,3)
#' invers_adjoint_matriks(x)
#' @param x variabel matriks persegi
#' @return Invers dengan metode adjoint matriks
#' @export


invers_adjoint_matriks <- function(x) {
  if(is.matrix(x) == FALSE) stop("variabel x harus dalam matriks")
  n <- nrow(x)
  m <- ncol(x)

  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")
  cat("Matriks awal\n")
  print(x)

  cof_matriks <- matrix(0, nrow = n, ncol = n)
  adj_matriks = 0
  if(n == 2){
    cat(paste0("\nTukar matriks[1][1] dengan matriks[2][2]\n"))
    temp = x[1,1]
    x[1,1] = x[2,2]
    x[2,2] = temp
    print(x)

    cat(paste0("\nMengganti tanda pada matriks[1][2] dengan matriks [2][1]\n"))
    x[1,2] = -1 * x[1,2]
    x[2,1] = -1 * x[2,1]
    print(x)

    adj_matriks <- x
    cat("\nHasil adjoint matriks yang didapatkan \n")
    print(adj_matriks)
  }
  else{
    for (i in 1:n) {
      for (j in 1:n) {
        minor <- x[-i, -j]
        cat(paste0("\nMinor ",i,",",j,"\n"))
        print(minor)

        cofactor = (-1)^(i+j) * det(minor)
        cat(paste0("cofactor ",i, ",",j," = (-1)^(",i,"+",j,") * ",det(minor), " = ",(-1)^(i+j)," * ",det(minor)," = ",cofactor,"\n"))
        cof_matriks[i, j] <- cofactor
      }
    }
    cat("\nMatriks kofaktor\n")
    print(cof_matriks)

    adj_matriks <- t(cof_matriks)
    cat("\nMelakukan transpose matriks kofaktor\n")
    print(adj_matriks)

    cat("\nHasil adjoint matriks yang didapatkan \n")
    print(adj_matriks)
  }

  cat(paste0("\nDeterminan matriks = ", det(x) ,"\n"))

  if(det(x) == 0) return("Karena determinan = 0, maka matriks tidak mempunyai invers")

  cat(paste0("\nInvers = 1/determinan(x) * adjoint_matriks\n"))

  invers = 1/det(x) * adj_matriks

  cat("\nHasil invers:\n")

  return(invers)
}

