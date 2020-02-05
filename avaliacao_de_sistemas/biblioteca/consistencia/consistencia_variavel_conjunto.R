#Consistencia alterada

#***
#rever funcao e proposta

#' consistencia_variavel_conjunto
#' @description 
#' funcao ainda em desenvolvimento, nao usar
#' @param tabela um dataframe sobre o qual serao realizadas as operacoes
#' @param colunaA a coluna de referencia
#' @param colunasB as colunas sobre as quais sera calculada a consitencia com relacao a \code{colunaA}
#' @param valores_padrao_A um conjunto de fatores de \code{colunaA} que serao considerados
#' @param valores_padrao_B um conjunto e conjunto de faotes para cada \code{colunasB}
#' @param diferenca ainda em implementacao
#' @return retorna a consistencia com relacao a cada variavel
#' @export
consistencia_variavel_conjunto <- function(tabela, colunaA, colunasB, valores_padrao_A, valores_padrao_B, diferenca = FALSE) {
  
  
  tabela <- subset(tabela, tabela[[colunaA]] %in% valores_padrao_A)
  
  filtrada <- tabela
  
  for(i in 1:length(colunasB)) {
    
    if(!diferenca) {
   
      print(valores_padrao_B[[i]])
      filtrada <- subset(filtrada, filtrada[[colunasB[i]]] %in% valores_padrao_B[[i]])

    } else {
      
      filtrada <- subset(filtrada, !(filtrada[[colunasB[i]]] %in% valores_padrao_B[[i]]))
      
    }
    
  }
  
  if(nrow(filtrada) != 0)
    return(nrow(tabela)/nrow(filtrada) * 100)
  else
    return(0)
  
}
