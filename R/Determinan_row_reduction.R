
#' Determinan dengan Row Reduction
#' @description
#' Fungsi ini akan melakukan pencarian nilai determinan dari suatu matriks persegi dengan menggunakan metode row reduction dan disertai dengan langkah pengerjaannya.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai determinan dengan menggunakan metode row reduction, Metode
#' row reduction dilakukan dengan cara transformasi matriks menjadi matriks echelon baris dan kemudian
#' menerapkan subtitusi mundur untuk mendapatkan nilai determinan.
#' @examples
#' x <- matrix(c(0,3,2,1,-6,6,5,9,1),3,3)
#' determinan_row_reduction(x)
#' @param x  matriks persegi
#' @return Nilai determinan dari matriks
#' @export

determinan_row_reduction <- function(x){
  if(is.matrix(x) == FALSE) stop("variabel x harus dalam matriks")
  n <- nrow(x)
  m <- ncol(x)
  if(n!=m) stop("Ukuran jumlah baris dan kolom harus sama (matriks persegi)")

  cat("Matriks awal\n")
  print(x)

  step <- 1
  change <- 0

  for (i in 1:n) {
    pivot_row <- i
    if(x[i,i] == 0){
      for (j in i:n) {
        if (abs(x[j, i]) >= abs(x[pivot_row, i])) {
          pivot_row <- j
        }
      }
      if (pivot_row != i) {
        x[c(i, pivot_row), ] <- x[c(pivot_row, i), ]
        cat(paste0("\nStep ", step ,": tukar baris ",i, " dengan baris ",pivot_row, "\n"))
        print(x)
        step = step + 1
        change = change + 1
      }
    }
  }

  idx <- c()
  index = 1
  for (i in 1:n) {
    pivot <- x[i, i]
    if (pivot == 0) {
      stop("Tidak dapat diselesaikan")
    }
    else x[i, ] <- x[i, ] / pivot

    cat(paste0("\nStep ", step, ": baris", i, " = baris ",i, " dibagi ", pivot,"\n"))
    print(x)
    step = step + 1
    idx[index] = pivot
    cat("\nCommon factor yang didapatkan :", idx[index], "\n")
    index = index + 1

    for (j in i:n) {
      if (i != j) {
        ratio <- x[j, i] / x[i, i]
        if(ratio == 0) next
        x[j, ] <- x[j, ] - ratio * x[i, ]
        cat(paste0("\nStep ", step,": baris ",j, " = ", -1*ratio, " * baris ", i, " + baris ", j, "\n"))
        print(x)
        step = step + 1
      }
    }
  }
  det = 1
  if(change %% 2 == 1) {
    print("karena terjadi pertukaran 2 baris maka determinan dikalikan -1")
    det = -1
    cat("\nDeterminan yang didapatkan =", det, "*",idx[1])
  }
  else {
    det = 1
    cat("\nDeterminan yang didapatkan =",idx[1])
  }


  for(i in 1:(index-1)){
    det <- det * idx[i]
    if(i > 1){
      cat(" *",idx[i])
    }
  }
  cat(" =",det,"\n")

  return(det)
}
