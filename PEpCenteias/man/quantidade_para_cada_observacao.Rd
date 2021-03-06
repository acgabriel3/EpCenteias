% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{quantidade_para_cada_observacao}
\alias{quantidade_para_cada_observacao}
\title{quantidade_para_cada_observacao}
\usage{
quantidade_para_cada_observacao(coluna, valoresPadrao = NULL, nota = NULL)
}
\arguments{
\item{valoresPadrao}{o conjunto de fatores que sera considerado para realizar a operacao}

\item{variavel}{recebe um vetor de fatores}
}
\value{
retorna a quantidade de cada fator de \code{valoresPadrao} em \code{variavel}
}
\description{
calcula com table do dplyr
}
\examples{
quantidade_para_cada_observacao(variavel = dengue2013$MUNICIPIO)
quantidade_para_cada_observacao(variavel = dengue2013$RESUL_SORO, valoresPadrao = c("2", "4"))
}
