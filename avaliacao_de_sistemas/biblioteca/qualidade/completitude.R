#***
#CAMINHO
#biblioteca/qualidade/completitude.R

library(xlsx)

#***
#QUALIDADE

# source("biblioteca/qualidade/completitude/completitude.R")

interface('completitude')


#***
#COMPOSTA PARA TABELA

#-Esta funcao recebe uma tabela inteira
#-Retorna uma tabela com a completitude de cada variavel da tabela
#-Funciona para valores em branco e NA
#-Tabela deve ser um data frame
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

