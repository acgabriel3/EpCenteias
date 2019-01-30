source("biblioteca.R")

completitude(dengue_2007$SEM_NOT)

valores <- c("masculino", "11")

camposDistintos(dengue_2007$RESUL_SORO, c("1", "2", "3","4"))

duracao_em_dias(dengue_2013$DT_NOTIFIC, dengue_2013$DT_SIN_PRI, quantidade_dias_referencia = 8, mediana = TRUE
                   ,valoresPadrao = c("2", "4"), variavel_identificador = dengue_2013$RESUL_SORO)
  
#***
#completitude para todas as variaveis de uma tabela
completitude_variaveis_de_uma_tabela(dengue_2007) #pode ser capturada a tabela com uma variavel,como tambem ela eh
                                                   #automaticamente escrita no formato xlsx

#***
#completitude apenas nos dados escolhidos de uma tabela
completitude_relacionada(dengue_2013$CS_SEXO, dengue_2013$CS_GESTANT, c("F"))


#***
#Validade para intervalo:
#-O valor a esquerda em valores padrao eh o limite inferior (por exemplo se quiser incluir 1000, coloque 999)
#-O valor a direita em valores padrao eh o limite superior (por exemplo, se quiser incluir 3000, coloque 3001)
camposDistintos(dengue_2013$NU_IDADE_N, valoresPadrao = c("1000", "3000"), intervalo = TRUE) 

#***
#Validade para data:
camposDistintos(dengue_2013$DT_NOTIFIC, data = TRUE) #funciona para formatos específicos (conversar sobre)

#***
#Contagem de quantidade de vezes que um municipio (por exemplo), aparece em uma variavel:
quantidade_para_cada_observacao(dengue_2013$MUNICIPIO)
quantidade_para_cada_observacao(dengue_2013$RESUL_SORO, c("2", "4"))

#***
#Consistencia para somente uma data
consistencia(dengue_2013$DT_SORO, dengue_2013$RESUL_SORO, duas_colunas_data = FALSE)

#Se for positivo possui um codigo especifico?
consistencia(dengue_2013$RESUL_SORO, dengue_2013$DT_SORO, duas_colunas_data = FALSE) 


#***
#Consistencia para valores determinados
#por codigo:
consistencia(dengue_2013$RESUL_SORO, dengue_2013$DT_SORO, valoresPadrao = c("2", "4"), duas_colunas_data = FALSE)

#***
#Consistencia para duas colunas sem valores determinados
consistencia(dengue_2013$RESUL_SORO, dengue_2013$DT_SORO, coluna_adicional_parametro = dengue_2013$NU_ANO, duas_colunas_data = FALSE)

#***
#Consistencia para duas colunas com valores padrao
consistencia(dengue_2013$RESUL_SORO, dengue_2013$DT_SORO,coluna_adicional_parametro = dengue_2013$NU_ANO,
             valoresPadrao = c("2", "4"), valores_padrao_adicionais =  c("2013"),duas_colunas_data = FALSE)


#***
#Representatividade para valores determinados
representatividade(dengue_2013$RESUL_SORO, c("2","4"))

#***
#Exemplo sensibilidade
porcentagem_verdadeiro_positivo(dengue_2013$RESUL_SORO, "2", "4") #supondo que 2 seja positivo e 4 seja falso positivo

#***
#Exemplo para representatividade condicionada aos valores de uma primeira coluna
representatividadeCondicionada(dengue_2013$RESUL_SORO, c("2","4"), dengue_2013$NU_ANO, c("2013"))

#***
#Exemplo para tabela representando medidas de acordo com os valores de uma coluna (como uma coluna de municipios)
tabela_variaveis_avaliacao(dengue_2013$CS_SEXO, dengue_2013$RESUL_SORO, representatividade, valoresPadrao = c("2"))

#***
#Exemplo para aplicar uma funcao em todas as colunas de uma tabela, sem valoresPadrao, e de acordo com um filtro
aplicacao_todas_as_colunas(dengue_2013$CS_SEXO, dengue_2013, completitude)

#***
#exemplo para aplicar uma funcao por uma variavel, por mes e qualquer base:

#Averiguar
tabela <- filtro_mes_variavel(tabelaA = dengue_2007, 
                              coluna_quantidade_somatorioA = "CS_GESTANT", 
                              colunafiltroA= "NU_ANO",
                              colunaDataA = "DT_NOTIFIC", 
                              meses = c(1:12),
                              anos = c(2007),
                              indicador = completitude)
