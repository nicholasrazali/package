
#' Determinan dengan ekspansi kofaktor
#' @description
#' Fungsi ini akan melakukan pencarian nilai determinan dari
#' suatu matriks persegi dengan menggunakan metode kofaktor dan disertai dengan langkah pengerjaannya.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai determinan dengan menggunakan
#' metode ekspansi kofaktor, Metode ekspansi kofaktor dilakukan dengan cara
#' melakukan ekspansi baris atau ekspansi kolom pada matriks.
#' @examples
#' x <- matrix(c(0,3,2,1,-6,6,5,9,1),3,3)
#' determinan_cofactor(x,n=1,by_row=TRUE)
#' @param x matriks persegi
#' @param n ekspansi kofaktor untuk elemen baris atau kolom ke berapa (default n = 1)
#' @param by_row apakah ekspansi kofaktor dilakukan secara ekspansi baris atau ekspansi kolom, jika TRUE maka dilakukan ekspansi baris, jika FALSE maka dilakukan ekspansi kolom (default by_row = TRUE)
#' @return Nilai determinan dari matriks
#' @export

determinan_cofactor <- function(x, n = 1, by_row = TRUE) {
  if(is.matrix(x) == FALSE) stop("variabel x harus dalam matriks")
  else if(!is.numeric(n) || length(n)!=1 || n>nrow(x)) stop("variabel n harus numeric dan tidak boleh lebih besar dari jumlah kolom/baris matriks x")
  else if(by_row != TRUE && by_row != FALSE) stop("by_row diisikan dengan TRUE untuk ekspansi baris, atau FALSE untuk ekspansi kolom")
  nr <- nrow(x)
  m <- ncol(x)
  if(nr!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")

  if(nr == 1) {
    cat(paste0("Determinan ", x[1,1], "\n"))
    return(x[1,1])
  } else if(nr == 2) {
    det <- x[1,1] * x[2,2] - x[1,2] * x[2,1]
    cat(paste0("Determinan = ( ", x[1,1], " * ", x[2,2], " ) - ( ",x[1,2], " * ", x[2,1], " ) = ", det, "\n"))
    return(det)
  } else {
    det <- c()
    det_idx = 1
    for(j in 1:nr) {
      if(by_row == TRUE){
        minor <- x[-n, -j]
        cat(paste0("\na",n,",",j," = ",x[n,j]))
        cat(paste0("\nMinor",n,",",j," : \n"))
        print(minor)

        determinan = determinan_cofactor(minor)
        cofactor <- (-1)^(n+j) * determinan
        cat(paste0("kofaktor untuk c",n,",",j, " = ",(-1)^(n+j), " * ", determinan, " = ", cofactor, "\n"))

        term <- x[n,j] * cofactor
        det[det_idx] = term
        det_idx = det_idx + 1

        cat(paste0("a",n,",",j, " * ", "c",n,",",j, " = ",x[n,j], " * ", cofactor, " = ",term, "\n" ))
        cat(paste0("Untuk ", n, ",", j, " = ", term, "\n"))
      }
      else{
        minor <- x[-j, -n]
        cat(paste0("\na",j,",",n," = ",x[j,n]))
        cat(paste0("\nMinor",j,",",n," : \n"))
        print(minor)

        determinan = determinan_cofactor(minor)
        cofactor <- (-1)^(j+n) * determinan
        cat(paste0("kofaktor untuk c",j,",",n, " = ",(-1)^(j+n), " * ", determinan, " = ", cofactor, "\n"))

        term <- x[j,n] * cofactor
        det[det_idx] = term
        det_idx = det_idx + 1

        cat(paste0("a",j,",",n, " * ", "c",j,",",n, " = ",x[j,n], " * ", cofactor, " = ",term, "\n" ))
        cat(paste0("Untuk ", j, ",", n, " = ", term, "\n"))
      }
    }

    det_final = det[1]
    cat(paste0("\nDeterminan yang didapatkan = ",det_final))
    for(i in 2:(det_idx-1)){
      det_final = det_final + det[i]
      cat(paste0(" + ", det[i]))
    }
    cat(paste0(" = ", det_final, "\n"))

    cat(paste0("\n","Determinan yang didapatkan = ", det_final, "\n"))
    return(det_final)
  }
}
