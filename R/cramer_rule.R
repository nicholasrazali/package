
#' Cramer Rule
#' @description
#' Fungsi ini untuk menyelesaikan sistem persamaan linear dengan menggunakan metode Cramer's Rule.
#'
#' @details
#' Fungsi ini untuk melakukan penyelesaian sistem persamaan linear dengan menggunakan Cramer's Rule,
#' yaitu dengan cara mencari determinan terlebih dahulu untuk masing-masing A_i, yaitu matriks yang kolom ke-i
#' digantikan dengan nilai dari vektor y, kemudian untuk penyelesaian persamaan linear
#' dilakukan dengan membagi hasil determinan pada matriks A_i dengan hasil determinan matriks awal (A).
#'
#' @examples
#' A <- matrix(c(1,-3,-1,0,4,-2,2,6,3),3,3)
#' b <- c(6,30,8)
#' cramer_rule(A,b)
#' @param A variable matriks persegi
#' @param b variabel vektor
#' @return Penyelesaian persamaan linear
#' @export


cramer_rule <- function(A,b){
  if(is.matrix(A) == FALSE) stop("variabel A harus dalam matriks")
  else if(is.vector(b) == FALSE) stop("variabel b harus dalam vector")

  n <- nrow(A)
  m <- ncol(A)
  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")
  else if(n != length(b)) stop("jumlah vector b tidak sama dengan jumlah baris dan kolom pada matriks")


  cat("Matriks awal A\n")
  print(A)

  det <- det(A)
  cat("\nDeterminan matriks awal =", det, "\n")
  if(det == 0) return(cat("\nKarena determinan matriks awal = 0 , maka terdapat banyak solusi"))

  dett <- c()
  for(i in 1:m){
    temp = A
    temp[,i] = b
    cat(paste0("\nMatriks A",i,"\n"))
    print(temp)
    dett[i] = det(temp)
    cat(paste0("\nDeterminan matriks A",i," = ", dett[i], "\n"))
  }

  x <- c()
  for(i in 1:m){
    x[i] = dett[i]/det
    cat(paste0("\nX",i," = ",dett[i],"/",det," = ", x[i]))
  }
  cat("\n\nHasil persamaan yang didapatkan \n")
  return(x)
}
