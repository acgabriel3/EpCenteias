

#' completitude_relacionada
#' calcula a completitude de uma variavel de acordo com um conjunto de fatores presentes em um conjunto de variaveis de referencia
#' @param tabela o dataframe em que os calculos serao realizados
#' @param variaveis_de_referencia o conjunto de variaveis utilizadas como referencia, quando combinadas devem indicar uma situacao
#' @param variavel_para_avaliacao a variavel sobre a qual será calculada a completitude
#' @param valoresPadrao um conjunto de vetores, com os fatores de cada uma das variáveis de referência, respectivamente
#' @example 
#' completitude_relacionada(dengue2013, ("CS_SEXO"), c("CS_GESTANT"), c(c("F")))
#' @return Retorna a completitude da \code{variavel_para_avaliacao} contendo apenas as linhas que possuiam determinados conjuntos de fatores
#' nas \code{variaveis_de_referencia} de acordo com os \code{valoresPadrao}
#' @export
completitude_relacionada <- function(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao = NULL
                                     
) { #variavel_de_referencia = n = variavel_para_avaliacao  valoresPadrao = m
  
  if(length(variaveis_de_referencia) != length(valoresPadrao) & !is.null(valoresPadrao)) {
    return(print("cada coluna deve conter os seus proprios valores padrao"))
  }
  
  for(i in (variaveis_de_referencia)) {
    tabela[[i]] <- as.character(tabela[[i]]) #complexidade n 
  }
  
  posicoes <- NULL
  
  if(!is.null(valoresPadrao)) {
    
    for(i in 1:length(variaveis_de_referencia)) {
      posicoes <- c(posicoes, which(tabela[[variaveis_de_referencia[i]]] %in% valoresPadrao[[i]]))
    }
    
    tabela <- tabela[unique(posicoes),] #complexidade n
    
  } else {
    
    tabela <- tabela[!is.na(tabela[[variaveis_de_referencia]]),] 
    
  }
  
  retorno <- completitude(tabela[[variavel_para_avaliacao]])
  
  return(retorno)
} #complexidade n
