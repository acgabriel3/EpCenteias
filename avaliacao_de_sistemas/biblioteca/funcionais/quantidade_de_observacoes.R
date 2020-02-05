#CAMINHO
#biblioteca/funcionais/quantidade_de_observacoes.R


# -Recebe uma variavel de uma tabela e valoresPadrao
# -Retorna a quantidade de vezes que a totalidade de valores padroes aparecem na tabela, 
#ou o total de observacoes na tabela (por padrao)

#' quantidade_de_observacoes
#' @description 
#' calcula com forca bruta
#' @param variavel recebe um vetor de fatores
#' @param valoresPadrao o conjunto de fatores que sera considerado para realizar a operacao
#' @return retorna a quantidade de cada fator de \code{valoresPadrao} em \code{variavel}
#' @examples 
#' quantidade_de_observacoes(variavel = dengue2013$MUNICIPIO)
#' quantidade_de_observacoes(variavel = dengue2013$RESUL_SORO, valoresPadrao = c("2", "4"))
#' @export
quantidade_de_observacoes <- function(variavel, valoresPadrao = NULL) { #variavel = n, valoresPadrao = m
  
  if(!is.null(valoresPadrao)) {
  
    variavel <- subset(variavel, variavel %in% valoresPadrao)  
    
  } 
    
  fatores <- as.factor(variavel)
  categorias <- levels(fatores)
  total <- 0
  
  for(i in 1:length(categorias)) {
    total <- total + sum(fatores == categorias, na.rm = TRUE) 
  } #complexidade mn
    
  return(total)
  
} #complexidade mn