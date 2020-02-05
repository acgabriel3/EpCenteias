
#-Esta funcao recebe uma coluna de uma tabela, e uma lista de valores que representa valores possiveis na coluna
#-retorna a porcentagem de observacoes da coluna que sao iguais aos valores setados

#' representatividade
#' @description
#' calcula a representatividade de um conjunto de fatores em uma variavel
#' @param variavel a variavel de um dataframe que contenha valores categoricos (fatores)
#' @param valoresPadrao o conjunto de fatores sobre os quais sera calculada a representatividade
#' @examples
#' representatividade(dengue2013$RESUL_SORO, c("2","4"))
#' @export
representatividade <- function(variavel, valoresPadrao) {#variavel = n, valoresPadrao = m

  # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n(chao de 2n)

  variavelFiltrada <- subset(variavel, variavel %in% valoresPadrao) #complexidade nm

  quantidadeDado <- length(variavelFiltrada) #complexidade n(chao de 2n)
  total <- length(variavel) #complexidade n

  retorno <- (quantidadeDado/total) * 100 #complexidade n
  return(paste("representatividade: ", retorno, "%", sep = ""))

} #complexidade nm
