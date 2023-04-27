
#' Dot Product Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai dot product vektor.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai dot product vektor.
#' @examples
#' u <- c(0,0,1)
#' v <- c(0,2,2)
#' alfa <- 45
#' dot_product(u,v,alfa)
#' @param u vektor input pertama
#' @param v vektor input pertama
#' @param alfa besaran sudut antara vektor u dan vektor v, jika vektor berada di n-space maka alfa = 0
#' @return Nilai dot product dari 2 vektor
#' @export

dot_product <- function(u, v, alfa = 0){
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u) != length(v)) stop("Jumlah data vektor yang dimasukkan harus sama")

  u1 = u^2
  v1 = v^2
  norm_u = sqrt(sum(u1))
  norm_v = sqrt(sum(v1))
  cos_alfa = cos(alfa * pi/180)
  if(alfa > 0){
    cat("Panjang vektor u =",norm_u, "\n")
    cat("Panjang vektor v =",norm_v, "\n")
    cat(paste0("Cos sudut = cos(",alfa,"* pi/180) = ", cos_alfa, "\n"))
    dot = norm_u * norm_v * cos_alfa
    cat("\ndot_product =",norm_u,"*",norm_v,"*",cos_alfa,"=",dot,"\n")
  }
  else {
    cat(paste0("dot_product = ",u[1],"*",v[1]))
    for(i in 2:length(u)){
      cat(paste0(" + ",u[i],"*",v[i]))
    }
    cat(" =", sum(u*v), "\n")
    dot = sum(u*v)
  }
  return(dot)
}
