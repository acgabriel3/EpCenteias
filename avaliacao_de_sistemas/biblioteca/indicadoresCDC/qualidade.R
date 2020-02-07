#***
#cAMINHO
#biblioteca/qualidade.R

#***
#Padrao de programacao:

#-O estilo dplyr eh preterivel ao estilo classico R, para facilitar a comunicacao entre programador e leigo (rever)
#-As funcoes que sera utilizadas pelos usuários finais podem conter ateh 5 palavras em sua constituicao. Devem
#ser o mais clara possíveis
#-toda variavel inicia com letra minuscula e eh separada por "_" caso seja um frase, e por uma letra maiuscula
#caso possua duas palavras
#-Toda funcao deve indicar explicitamente as variaveis de entrada, o que faz e o formato das variaveis de entrada
#-O nome das variaveis e das funcoes nao pode conter mais de 3 palavras
#-Toda funcao deve conter um comentario explicando sua funcionalidade
#-Todo passo nao claro deve conter um comentario explicando a sua funcionalidade
#-Todo comentario referente ao codigo que nao se relacionam ao funcionamento logico de alguma parte integrante
#deve conter um comentario anterior no seguinte formato: " #*** "
#-Todo passo, loop e funcao deve apresentar um comentario indicando qual a sua complexidade

#***
#Funcoes irmas
# source("biblioteca/funcionais.R")

#***
#QUALIDADE

# source("biblioteca/qualidade/completitude.R")

interface('qualidade')

