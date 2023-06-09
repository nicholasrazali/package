% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_product.R
\name{cross_product}
\alias{cross_product}
\title{Cross Product Vektor}
\usage{
cross_product(u, v)
}
\arguments{
\item{u}{vektor input pertama}

\item{v}{vektor input pertama}
}
\value{
Plot cross product dari 2 vektor
}
\description{
Fungsi ini akan melakukan pencarian cross product vektor.
}
\details{
Fungsi ini untuk melakukan pencarian cross product vektor, dengan syarat bahwa
vektor harus berada pada 3 dimensi. Kemudian juga akan ditampilkan ke dalam plot grafik.
}
\examples{
u <- c(0,0,1)
v <- c(0,2,2)
cross_product(u,v)
}
