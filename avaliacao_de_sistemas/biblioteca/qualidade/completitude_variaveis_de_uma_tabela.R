
#***
#COMPOSTA PARA TABELA

#-Esta funcao recebe uma tabela inteira
#-Retorna uma tabela com a completitude de cada variavel da tabela
#-Funciona para valores em branco e NA
#-Tabela deve ser um data frame

#' completitude_variaveis_de_uma_tabela
#' @description 
#' Calcula a completitude de todas as variaveis (colunas) de um dataframe
#' @param tabela um dataframe sobre o qual sera realizada a operacao
#' @param registrar indica se o resultado final deve ser registrado em um documento no formato xlsx 
#' @return retorna um dataframe com a completitude de todas as variaveis de uma tabela
#' @examples 
#' completitude_variaveis_de_uma_tabela(tabela = dengue2013)
#' completitude_variaveis_de_uma_tabela(tabela = dengue2013, registrar = TRUE)
#' @export
completitude_variaveis_de_uma_tabela <- function(tabela, registrar = FALSE) { #tabela linhas = n tabela colunas = m
  
  numLinhas <- length(tabela) #complexidade m
  nomeColunas <- colnames(tabela) #complexidade m
  
  tabVariavel_x_completitude <- data.frame(variavel = c(1:numLinhas), completitude = NA) #complexidade inteira
  
  
  for(i in 1:numLinhas) {
    
    tabVariavel_x_completitude$variavel[i] <- nomeColunas[i] #complexidade 1
    tabVariavel_x_completitude$completitude[i] <- completitude(as.vector(tabela[[as.character(nomeColunas[i])]])) #complexidade m + 4n
    
  } #complexidade 4nm + mQuadrado
  
  if(registrar)
    write.xlsx(tabVariavel_x_completitude, "completitude_variaveis.xlsx")
  
  return(tabVariavel_x_completitude)
  
} #complexidade nm(chao de 4nm) ou mQuadrado (depende do qtd de variaveis) ou nm + mQuadrado

