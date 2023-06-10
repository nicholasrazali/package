GaussJordan <- function(x, baris=TRUE, r=5){
  list(
    x.asal <<- x,
    tukar = function(i,j){
      if(is.matrix(x)){
        if(baris){
          ind <- diag(nrow(x))
        }
        else ind <- diag(ncol(x))
      }
      else stop("Objek harus dalam mode matriks")

      te <- ind[i,]
      ind[i,] <- ind[j,]
      ind[j,] <- te

      if(baris){
        x <<- ind%*%x
      }
      else x <<- x%*%ind

      return(x)
    },
    kali = function(i,k){
      if(is.matrix(x)){
        if(baris){
          ind <- diag(nrow(x))
        }
        else ind <- diag(ncol(x))
      }
      else stop("Objek harus dalam mode matriks")

      ind[i,] <- k*ind[i,]
      if(baris) x<<- ind %*% x
      else x <<- x %*% ind

      return(x)
    },
    lipat = function(i, j, k){
      if(is.matrix(x)){
        if(baris){
          ind <- diag(nrow(x))
        }
        else ind <- diag(ncol(x))
      }
      else stop("Objek harus dalam mode matriks")

      if(baris){
        ind[j,i] <- k
        x <<- ind %*% x
      }
      else{
        ind[i,j] <- k
        x <<- x %*% ind
      }
      return(x)
    },
    hasil = function(){
      x.setara <- x
      has <- list(Matriks.Asal=x.asal, Matriks.Setara=round(x.setara,r))
      return(has)
    }
  )
}
