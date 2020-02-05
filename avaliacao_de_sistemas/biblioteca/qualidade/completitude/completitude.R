#***
#CAMINHO
#biblioteca/qualidade/completitude/completitude.R


#-Esta funcao recebe uma coluna referente a alguma variavel de uma tabela
#-Retorna a proporcao de valores vazios dessa coluna pelo total de linhas da coluna
#-Variavel deve ser uma coluna de um data frame, ou um vetor

#' completitude
#' @param variavel um vetor, ou variavel (coluna) de um datafram sobre a qual a operacao sera realizada
#' @return retorna o calculo da completitude de \code{variavel}
#' @examples
#' completitude(dengue2013$SEM_NOT)
#' @export
completitude <- function(variavel) { #variavel = n
  
  # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n(chao de 2n)
  
  linhasVazias <- sum(is.na(variavel), na.rm = TRUE) #complexidade n
  totalLinhas <- length(variavel) #complexidade n
  
  resultado <- (linhasVazias/totalLinhas) * 100 #complexidade n(chao de 2n)
  
  return(resultado)
  
} #complexidade n(chao de 2n)
