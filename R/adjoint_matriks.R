
#' Pembentukan Adjoint Matriks
#' @description
#' Fungsi ini akan melakukan pembentukan adjoint matriks.
#'
#' @details
#' Fungsi ini untuk melakukan pembentukan adjoint matriks, yaitu dengan membuat matriks kofaktor sesuai
#' dengan nilai dari kofaktor untuk baris dan kolom matriks tersebut kemudian dikalian dengan
#' (-1) pangkat penjumlahan dari baris dan kolom tesebut. Setelah mendapatkan
#' matriks kofaktor, selanjutnya melakukan transpose dari matriks kofaktor untuk mendapatkan
#' adjoint matriks.
#' @examples
#' x <- matrix(c(0,3,2,1,-6,6,5,9,1),3,3)
#' adjoint_matriks(x)
#' @param x variabel matriks persegi
#' @return Adjoint matriks
#' @export


adjoint_matriks <- function(x) {
  if(is.matrix(x) == FALSE) stop("variabel x harus dalam matriks")
  n <- nrow(x)
  m <- ncol(x)

  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")
  cat("Matriks awal\n")
  print(x)

  cof_matriks <- matrix(0, nrow = n, ncol = n)

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
  return(adj_matriks)
}

