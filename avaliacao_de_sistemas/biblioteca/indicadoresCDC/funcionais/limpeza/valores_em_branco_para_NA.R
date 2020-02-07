#***
#CAMINHO
#biblioteca/funcionais/limpeza/valores_em_branco_para_NA.R

#***
#FUNCIONAIS

#-Esta funcao recebe uma coluna referente a alguma variavel de uma tabela
#-Retorna uma variavel modificada com todos o valores em branco substituidos por NA
#-Variavel deve ser a coluna de um data frame ou um vetor

#' valores_em_branco_para_NA
#' @param um vetor sobre o qual sera realizada a operacao
#' @return retorna o vetor, com todas as observacoes que estavam em branco trocados por NA
#' @examples
#' valores_em_branco_para_NA(dengue2013$DT_NOTIFIC)
#' @export
valores_em_branco_para_NA <- function(variavel) {
  
  variavel <- as.character(variavel)
  
  variavel[variavel == ""] <- NA #complexidade 2n
  return(variavel)
  
} #complexidade n(chao de 2n)