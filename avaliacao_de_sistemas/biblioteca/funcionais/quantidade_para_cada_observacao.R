#***
#CAMINHO
#biblioteca/funcionais/aplicacao/aplicacaoBase/quantidade_para_cada_observacao.R

library(xlsx)

#***
#FUNCAO CAULE
#-Esta funcao recebe a coluna de uma tabela
#-Retorna a quantidade de vezes que cada variavel aparece na tabela

#' quantidade_para_cada_observacao
#' @description 
#' calcula com table do dplyr
#' @param variavel recebe um vetor de fatores
#' @param valoresPadrao o conjunto de fatores que sera considerado para realizar a operacao
#' @return retorna a quantidade de cada fator de \code{valoresPadrao} em \code{variavel}
#' @examples 
#' quantidade_para_cada_observacao(variavel = dengue2013$MUNICIPIO)
#' quantidade_para_cada_observacao(variavel = dengue2013$RESUL_SORO, valoresPadrao = c("2", "4"))
#' @export
quantidade_para_cada_observacao <- function(coluna, valoresPadrao = NULL) { # coluna = n
  
  if(!is.null(valoresPadrao)) {
    coluna <- subset(coluna, coluna %in% valoresPadrao)
  }
  
  observacoes <- as.factor(coluna) #complexidade n
  retorno <- data.frame(table(observacoes)) #complexidade 2n
  write.xlsx(retorno, "quantidade_para_cada_observacao.xlsx")
  return(retorno)
  
} #complexidade n(chao de 2n)
