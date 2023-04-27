
#' Proyeksi Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai proyeksi skalar dan proyeksi ortogonal.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian pencarian nilai proyeksi skalar dan proyeksi ortogonal,
#' dimana untuk dua vektor yang tidak berhimpit sehingga salah satu vektor bisa
#' diproyeksikan ke vektor lainnya.
#' @examples
#' u <- c(0,0,1)
#' a <- c(0,2,2)
#' proyeksi_vektor(u,a)
#' @param u vektor input pertama
#' @param a vektor input pertama
#' @return Nilai proyeksi skalar dan proyeksi ortogonal.
#' @export

proyeksi_vektor <- function(u, a){
  if(!is.vector(u) || !is.vector(a))stop("Input data harus berupa vektor")
  else if(length(u) != length(a)) stop("Jumlah data vektor yang dimasukkan harus sama")

  cat(paste0("ua = ",u[1],"*",a[1]))

  for(i in 2:length(u)){
    cat(paste0(" + ",u[i],"*",a[i]))
  }
  cat(" =", sum(u*a), "\n")

  ua = sum(u*a)
  norm_a2 =sum(a^2)
  cat(paste0("norm a^2 = ",norm_a2),"\n")

  skalar = ua/norm_a2
  cat(paste0("\nskalar = ",ua,"/",norm_a2," = ", skalar, "\n"))

  ortogonal = skalar * a
  cat(paste0("\nortogonal = ", skalar, " * (",a[1]))
  for(i in 2:length(a)){
    cat(paste0(", ",a[i]))
  }
  cat(") = (", ortogonal[1])
  for(i in 2:length(ortogonal)){
    cat(paste0(", ",ortogonal[i]))
  }
  cat(")","\n\n")

  return(list("proyeksi skalar vektor u pada vektor a" = skalar,"proyeksi vektor u pada vektor a" = ortogonal))
}
