% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code.R
\name{consistencia_variavel_conjunto}
\alias{consistencia_variavel_conjunto}
\title{consistencia_variavel_conjunto}
\usage{
consistencia_variavel_conjunto(
  tabela,
  colunaA,
  colunasB,
  valores_padrao_A,
  valores_padrao_B,
  diferenca = FALSE,
  nota = NULL
)
}
\arguments{
\item{tabela}{um dataframe sobre o qual serao realizadas as operacoes}

\item{colunaA}{a coluna de referencia}

\item{colunasB}{as colunas sobre as quais sera calculada a consitencia com relacao a \code{colunaA}}

\item{valores_padrao_A}{um conjunto de fatores de \code{colunaA} que serao considerados}

\item{valores_padrao_B}{um conjunto e conjunto de faotes para cada \code{colunasB}}

\item{diferenca}{ainda em implementacao}
}
\value{
retorna a consistencia com relacao a cada variavel
}
\description{
funcao ainda em desenvolvimento, nao usar
}
