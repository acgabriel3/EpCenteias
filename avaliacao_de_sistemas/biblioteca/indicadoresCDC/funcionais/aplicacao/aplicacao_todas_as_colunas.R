#***
#CAMINHO
#biblioteca/funcionais/aplicacao/aplicacaoBase/quantidade_para_cada_observacao.R

library(xlsx)

# source("biblioteca/funcionais/aplicacao/aplicacaoBase.R")

#' aplicacao_todas_as_colunas
#' @description 
#' funcao em desenvolvimento que aplica uma funcao a todas as colunas de uma tabela de acordo com uma coluna referencia
#' @param colunaBase uma variavel de fatores de um dataframe 
#' @param tabela um dataframe sobre o qual serao realizadas as operacoes
#' @param funcao a funcao que sera aplicada sobre as colunas de \code{tabela}
#' @param valoresPadrao o conjunto de valores que sera considerado para a aplicacao da funcao
#' @return retorna os resultados da aplicacao da funcao em cada coluna
#' @export
aplicacao_todas_as_colunas <- function(colunaBase, tabela, funcao, valoresPadrao = NULL) {
  
  retorno <- NULL  
  
  if(!is.null(valoresPadrao)) {
    
    if(length(valoresPadrao) > 1) {
      
      if(length(valoresPadrao) != length(tabela)) {
        return(print("valoresPadrao precisa possuir o mesmo numero de colunas que a tabela"))
      }
      
    }
    
  }
    
  for(i in 1:length(tabela)) {
     
    if(is.null(valoresPadrao)) {
          
        registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao)
        registro["coluna"] <- colnames(tabela)[i]
        retorno <- rbind(retorno, registro)

    }else if(length(valoresPadrao) == length(tabela)) {
      
      registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao, valoresPadrao[i])
      registro["coluna"] <- colnames(tabela)[i]
      retorno <- rbind(retorno, registro)
      
    } else {
    
        registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao, valoresPadrao)
        registro["coluna"] <- colnames(tabela)[i]
        retorno <- rbind(retorno, registro)
        
    }
     
  }
  
  write.xlsx(retorno, "tabela_gerada.xlsx")
  return(retorno)
  
}
