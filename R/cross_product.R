
#' Cross Product Vektor
#' @description
#' Fungsi ini akan melakukan pencarian nilai dot product vektor.
#'
#' @details
#' Fungsi ini untuk melakukan pencarian nilai dot product vektor.
#' pada matriks.
#' @examples
#' u <- c(0,0,1)
#' v <- c(0,2,2)
#' cross_product(u,v)
#' @param u vektor input pertama
#' @param v vektor input pertama
#' @return Nilai cross product dari 2 vektor
#' @export

cross_product <- function(u, v) {
  if(!is.vector(u) || !is.vector(v))stop("Input data harus berupa vektor")
  else if(length(u) != 3 || length(v) != 3) stop("Vektor harus memiliki sejumlah 3 data")
  cat(paste0("cross_product = ((",u[2], "*",v[3] ," - ", u[3], "*",v[2],"),(",u[3], "*",v[1] ," - ", u[1], "*",v[3],"),(",u[1], "*",v[2] ," - ", u[2], "*",v[1],"))\n"))
  cat(paste0(" = ((",u[2]*v[3], " - ", u[3]*v[2],"), (",u[3]*v[1]," - ",u[1]*v[3],"), (",u[1]*v[2]," - ",u[2]*v[1],"))\n" ))
  cat(paste0(" = (",u[2]*v[3] - u[3]*v[2],", ",u[3]*v[1] - u[1]*v[3], ", ",u[1]*v[2] - u[2]*v[1],")\n"))

  cross = c(u[2]*v[3] - u[3]*v[2], u[3]*v[1] - u[1]*v[3], u[1]*v[2] - u[2]*v[1])
  return(cross)
}
