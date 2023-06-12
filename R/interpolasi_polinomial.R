
#' Interpolasi Polinomial
#' @description
#' Fungsi ini akan melakukan pencarian interpolasi dengan persamaan polinomial secara reduksi baris.
#'
#' @details
#' FUngsi ini untuk menemukan persamaan interpolasi polinomial dengan menggunakan reduksi baris
#' pada augmented matriks. Dimana untuk kolom pertama dari matriks yang dibuat berupa angka
#' 1, kemudian untuk kolom lainnya diisikan sesuai dengan persamaan polinomial sebanyak jumlah
#' vektor x yang dimasukkan dikurangi 1 (n-1), dan untuk kolom terakhir akan digabungkan dengan
#' nilai pada vektor y.
#' @export
#' @examples
#' x <- c(1,2,3,4)
#' y <- c(3,-2,-5,0)
#' interpolasi_polinomial(x,y)
#' @param x variabel vektor untuk koordinat x
#' @param y variabel vektor untuk koordinat y
#' @return Persamaan interpolasi polinomial


interpolasi_polinomial <- function(x, y) {
  if(!is.vector(x) || !is.vector(y)) stop("Kedua variabel harus dalam bentuk vektor")

  len_x <- length(x)
  len_y <- length(y)
  if(len_x != len_y) stop("Jumlah vektor x dan vektor y harus sama")

  A <- matrix(rep(1,len_x),len_x,1)

  for(i in 1:(len_x - 1)){
    A <- cbind(A, as.matrix(x^i))
  }
  n <- nrow(A)
  augmented_mat   <- cbind(A, y)
  cat("Matriks awal\n")
  print(A)

  cat("\nStep 1: Menjadikan Augmented Matrix\n")
  print(augmented_mat)
  step <- 2

  for (i in 1:n) {
    pivot <- augmented_mat[i, i]
    if (pivot == 0) {
      if(i == n) return(cat("\n\nTidak dapat menemukan interpolasi"))
      else{
        for (j in (i+1):n) {
          if (abs(augmented_mat[j, i]) != 0) {
            augmented_mat[c(i, j), ] <- augmented_mat[c(j, i), ]
            cat(paste0("\nStep ", step ,": tukar baris ",i, " dengan baris ",j, "\n"))
            print(augmented_mat)
            step = step + 1
            next
          }
          if (j == n) return(cat("\n\nTidak dapat menemukan interpolasi"))
        }
      }
    }

    pivot <- augmented_mat[i, i]
    augmented_mat[i, ] <- augmented_mat[i, ] / pivot
    if(pivot != 1){
      cat(paste0("\nStep ", step, ": baris ", i, " = baris ",i, " dibagi ", pivot,"\n"))
      print(augmented_mat)
      step = step + 1
    }
    for (j in i:n) {
      if (i != j) {
        ratio <- augmented_mat[j, i] / augmented_mat[i, i]
        if(ratio == 0) next
        augmented_mat[j, ] <- augmented_mat[j, ] - ratio * augmented_mat[i, ]
        cat(paste0("\nStep ", step,": baris ",j, " = ", -1*ratio, " * baris ", i, " + baris ", j, "\n"))
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  for (i in (n-1):1) {
    for (j in (i+1):n) {
      if (i != j) {
        ratio <- augmented_mat[i, j] / augmented_mat[j, j]
        if(ratio == 0) next
        augmented_mat[i, ] <- augmented_mat[i, ] - ratio * augmented_mat[j, ]
        cat(paste0("\nStep ", step,": baris ",i, " = ", -1*ratio, " * baris ", j, " + baris ", i, "\n"))
        print(augmented_mat)
        step = step + 1
      }
    }
  }
  x <- augmented_mat[, n + 1]

  cat("\nBentuk persamaan  = ")
  if(x[1] != 0) cat(paste0(x[1]," "))

  for(i in 2:length(x)){
    if(x[i]==0) next
    else if(x[i]<0) {
      if(i == 2) cat(paste0("- ", -1*x[i],"x "))
      else cat(paste0("- ", -1*x[i],"x^",i-1," "))
    }else {
      if(i == 2) cat(paste0("+ ", x[i],"x "))
      else cat(paste0("+ ", x[i],"x^",i-1," "))
    }
  }

  cat("\n\nHasil persamaan yang didapatkan \n")
  return(x)
}
