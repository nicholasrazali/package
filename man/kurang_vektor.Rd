% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kurang_vektor.R
\name{kurang_vektor}
\alias{kurang_vektor}
\title{Menghitung Pengurangan 2 vektor}
\usage{
kurang_vektor(u, v)
}
\arguments{
\item{u}{vektor input pertama}

\item{v}{vektor input kedua}
}
\value{
Plot pengurangan dari vektor u dan vektor v.
}
\description{
Fungsi ini akan melakukan pengurangan dari 2 vektor dan menghasilkan hasil plot grafik.
}
\details{
Fungsi ini untuk menghitung pengurangan dari 2 vektor yang diinput.
Dimana dilakukan pengecekan terhadap jumlah data dari vektor yang dimasukkan. Kemudian hasil akhir
akan ditampilkan juga dalam bentuk plot grafik.
}
\examples{
u <- c(1,4)
v <- c(5,0)
kurang_vektor(u,v)
}
