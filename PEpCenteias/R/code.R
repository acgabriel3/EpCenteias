
 #Consistencia alterada
 
 
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
 #***
 #CAMINHO
 #biblioteca/funcionais/aplicacao/aplicacaoBase/quantidade_para_cada_observacao.R
 
 library(xlsx)
 # source("biblioteca/funcionais/aplicacao/aplicacaoBase.R")
 
 aplicacao_todas_as_colunas <- function(colunaBase, tabela, funcao, valoresPadrao = NULL) {
   
   retorno <- NULL  
   
   if(!is.null(valoresPadrao)) {
     
     if(length(valoresPadrao) > 1) {
       
       if(length(valoresPadrao) != length(tabela)) {
         return(print("valoresPadrao precisa possuir o mesmo numero de colunas que a tabela"))
       }
       
     }
     
   }
     
   for(i in 1:length(tabela)) {
      
     if(is.null(valoresPadrao)) {
           
         registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao)
         registro["coluna"] <- colnames(tabela)[i]
         retorno <- rbind(retorno, registro)
 
     }else if(length(valoresPadrao) == length(tabela)) {
       
       registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao, valoresPadrao[i])
       registro["coluna"] <- colnames(tabela)[i]
       retorno <- rbind(retorno, registro)
       
     } else {
     
         registro <- tabela_variaveis_avaliacao(colunaBase, tabela[[i]], funcao, valoresPadrao)
         registro["coluna"] <- colnames(tabela)[i]
         retorno <- rbind(retorno, registro)
         
     }
      
   }
   
   write.xlsx(retorno, "tabela_gerada.xlsx")
   return(retorno)
   
 }
 #CAMINHO
 #biblioteca/funcionais/tabela_variaveis_avaliacao.R
 
 library(xlsx)
 # source("biblioteca/funcionais/limpeza/valores_em_branco_para_NA.R")
 
 
 #-Esta funcao recebe duas variaveis de mesmo tamanho de uma tabela ou duas, uma funcao da biblioteca de avalicao 
 #e valores padrao para serem aplicados na coluna alvo
 #-Retorna e registra em formato excel uma tabela com a medida aplicada a cada observacao diferente de variavelBase
 #-VariavelBase e variavel devem ser uma coluna de uma data frame, ou um vetor
 #-Funcao deve ser uma funcao aplicada a uma coluna, que possua ou nao valoresPadrao como parametro
   #Se nao forem informados valores padrao, a funcao sera aplicada apenas recebendo variavel como parametro
 #-ValoresPadrao deve ser um vetor
 tabela_variaveis_avaliacao <- function(variavelBase, variavel, funcao, valoresPadrao = NULL) { #variavelBase = n = variavel, levels(variavelBase) = m, valoresPadrao = k
   
   # variavelBase <- valores_em_branco_para_NA(variavelBase) #complexidade n
   aux_variavel_base <- as.factor(variavelBase) #complexidade n
   distintos <- levels(aux_variavel_base) #complexidade n
   colunas <- c(distintos) 
   aux <- data.frame(matrix(ncol = length(colunas), nrow = 1))
   colnames(aux) <- colunas
 
   for(i in 1:length(distintos)) {
     filtro <- variavel[variavelBase == distintos[i]] #complexidade 2n
     
     if(is.null(valoresPadrao)) {
       medida <- funcao(filtro) #complexidade polinomial
     } else {
       medida <- funcao(filtro, valoresPadrao = valoresPadrao) #complexidade mXpolinomialXk
     }
     
     aux[1,i] <- medida
   } #complexidade mXpolinomial ou mXpolinomalXk
   
   #write.xlsx(retorno, "tabela_variaveis_avaliacao.xlsx")
   return(aux)
   
 } #complexidade mXpolinomial ou mXpolinomalXk
 #***
 #CAMINHO
 #biblioteca/funcionais/aplicacao/filtro_mes_servico.R
 
 library(lubridate)
 #- Esta funcao recebe de uma a tres tabelas, respectivamente com: 1 a 3 nomes de colunas
 #com informacoes, 1 a 3 nomes de colunas acerca de data, e 1 a 3 colunas acerca de valores 
 #categoricos. Recebe valoresPadrao, casos, meses, e anos.
 #- Esta funcao filtra segundo os anos e meses dados, as colunas de informacao, segundo
 #as categorias das colunas categoricas, e aplica uma funcao qualquer a estes dados. 
 #(deve-se preencher corretamente a funcao, de acordo com a funcao que sera aplicada (indicador))
 #- Retorna uma tabela com os resultados da funcao aplicada para cada categoria e data escolhida.
 filtro_mes_variavel <- function(tabelaA, coluna_quantidade_somatorioA, tabelaB = NULL, coluna_quantidade_somatorioB = NULL,
                                tabelaC = NULL, coluna_quantidade_somatorioC = NULL, 
                                colunafiltroA, colunafiltroB, colunafiltroC, 
                                colunaDataA, colunaDataB,colunaDataC = NULL, meses, anos, indicador, valores_padrao_aplicados = NULL, 
                                casos = NULL, valoresPadrao = NULL
                                ) { #linhas tabela = n, meses = k, anos = t, indicador = "complexidade"
   
   aux <- data.frame(servicio = NA, data = NA, indicador = NA) #complexidade 1
   retorno <- NULL #complexidade 1
   print("1") 
   tabelaA[colunaDataA] <- as.Date(tabelaA[[colunaDataA]]) #complexidade n
   print("2")
   if(!is.null(tabelaB)) {
     tabelaB[colunaDataB] <- as.Date(tabelaB[[colunaDataB]]) #complexidade n
   }
   if(!is.null(colunaDataC)) {
     tabelaC[colunaDataC] <- as.Date(tabelaC[[colunaDataC]]) #complexidade n
   }
   print("2")
   
   valores <- as.vector(levels(as.factor(tabelaA[[colunafiltroA]]))) #complexidade n, gera-se valores = m
   print(valores)
   
   for(i in 1:length(valores)) { #m vezes
     print("3")
     
     tabelaFiltradaA <- subset(tabelaA, tabelaA[[colunafiltroA]] %in% valores[i]) #complexidade n
     
     if(!is.null(tabelaB)) {
       tabelaFiltradaB <- subset(tabelaB, tabelaB[[colunafiltroB]] %in% valores[i])
     }
     
     if(!is.null(tabelaC)) {
       tabelaFiltradaC <- subset(tabelaC, tabelaC[[colunafiltroC]] %in% valores[i])
     }
     
     for(ano in anos) { #k vezes
       for(mes in meses) { #t vezes
      
         tabela_filtradaA <- subset(tabelaFiltradaA, (month(as.POSIXlt(tabelaFiltradaA[[colunaDataA]]))== mes) &
                                      year(as.POSIXlt(tabelaFiltradaA[[colunaDataA]]))== ano) #complexidade n
         
         if(!is.null(tabelaB)) {
           tabela_filtradaB <- subset(tabelaFiltradaB, (month(as.POSIXlt(tabelaFiltradaB[[colunaDataB]]))== mes) &
                                        year(as.POSIXlt(tabelaFiltradaB[[colunaDataB]]))== ano)
         }
         if(!is.null(tabelaC)) {
           tabela_filtradaC <- subset(tabelaFiltradaC, (month(as.POSIXlt(tabelaFiltradaC[[colunaDataC]]))== mes) &
                                        year(as.POSIXlt(tabelaFiltradaC[[colunaDataC]]))== ano)
         }
         
         
         if(!is.null(coluna_quantidade_somatorioB) & is.null(coluna_quantidade_somatorioC)) {
           if(is.null(casos) & is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                       tabela_filtradaB[[coluna_quantidade_somatorioB]]) 
             
           } else if(!is.null(casos) & !is.null(valoresPadrao)){
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]], casos = casos, valoresPadrao = valoresPadrao)
             
           } else if(!is.null(casos)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]], casos = casos)
             
           } else if(!is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]], valoresPadrao = valoresPadrao)
             
           }
         } else if(!is.null(coluna_quantidade_somatorioC)){
           
           if(is.null(casos) & is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]],
                                    tabelaFiltradaC[[coluna_quantidade_somatorioC]])
             
           } else if(!is.null(casos) & !is.null(valoresPadrao)){
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]],
                                    tabela_filtradaC[[coluna_quantidade_somatorioC]], casos = casos, valoresPadrao = valoresPadrao)
             
           } else if(!is.null(casos)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]],
                                    tabela_filtradaC[[coluna_quantidade_somatorioC]], casos = casos)
             
           } else if(!is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]],
                                    tabela_filtradaB[[coluna_quantidade_somatorioB]],
                                    tabela_filtradaC[[coluna_quantidade_somatorioC]], valoresPadrao = valoresPadrao)
             
           }
           
         } else {
           
           if(is.null(casos) & is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]])
             
           } else if(!is.null(casos) & !is.null(valoresPadrao)){
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]], casos = casos, valoresPadrao = valoresPadrao)
             
           } else if(!is.null(casos)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]], casos = casos)
             
           } else if(!is.null(valoresPadrao)) {
             
             resultado <- indicador(tabela_filtradaA[[coluna_quantidade_somatorioA]], valoresPadrao = valoresPadrao)
             
           }
           
         }
         
         
         aux[1,1] <- valores[i]
         if(mes < 10) {
           aux[1,2] <- paste(ano,"/", "0" ,mes, "/","01", sep ="")
         } else {
           aux[1,2] <- paste(ano,"/",mes, "/","01", sep ="")
         }
         aux[1,3] <- resultado
         retorno <- rbind(retorno, aux)
         
       }
   }
     
   }
   
   return(retorno)
   
 } #complexidade mkt X max(n, complexidadeIndicador), sendo k < 12 (pode ser quadratica em alguns casos, porem na maioria dos casos eh aprox quadratica)
 #tabela <- filtro_mes_variavel(tabelaA = material, 
 #                 coluna_quantidade_somatorioA = "KA_CANTID", 
 #                 tabelaB = hospitalizacion, 
 #                 coluna_quantidade_somatorioB = "HO_DIASH", 
 #                 colunafiltroB = "HO_CSERV", 
 #                 colunafiltroA= "KA_CSER",
 #                 colunaDataA = "KA_FECDES", 
 #                 colunaDataB = "HO_FHOSP", 
 #                 meses = c(1:6),
 #                 anos = c(2018),
 #                 indicador = indicadorConsumo)
 #Objetivo: Criar uma funcao para aplicar outras funcoes a todas as colunas de uma tabela. 
 
 #***
 #Padronizar entradas em todas as funcoes da biblioteca.
 #***
 #Seria necessario colocar todas as possibilidades possiveis de entrada, talvez seja possivel 
 #modularizar este processo (pensar acerca)
 #***
 #Posso utilizar aqui um swich
 
 aplicador_tabelas <- function(tabela, funcao, funcao2 = NULL, valoresPadrao = NULL, 
                               colunaBase = NULL) #conjunto de variaveis possiveis) 
   {
  
   if(!is.null(funcao2) || !is.null(valoresPadrao) || !is.null(colunaBase))
   sapply(tabela, 
                     function(X) {
                         print(funcao(colunaBase, X, funcao2))
                         }
          )                             
                                
 }
 #***
 #CAMINHO
 #biblioteca/funcionais/limpeza/valores_em_branco_para_NA.R
 
 #***
 #FUNCIONAIS
 
 #-Esta funcao recebe uma coluna referente a alguma variavel de uma tabela
 #-Retorna uma variavel modificada com todos o valores em branco substituidos por NA
 #-Variavel deve ser a coluna de um data frame ou um vetor
 
 valores_em_branco_para_NA <- function(variavel) {
   
   variavel <- as.character(variavel)
   
   variavel[variavel == ""] <- NA #complexidade 2n
   return(variavel)
   
 } #complexidade n(chao de 2n)
 #CAMINHO
 #biblioteca/funcionais/quantidade_de_observacoes.R
 
 
 # -Recebe uma variavel de uma tabela e valoresPadrao
 # -Retorna a quantidade de vezes que a totalidade de valores padroes aparecem na tabela, 
 #ou o total de observacoes na tabela (por padrao)
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
 #***
 #CAMINHO
 #biblioteca/funcionais/aplicacao/aplicacaoBase/quantidade_para_cada_observacao.R
 
 library(xlsx)
 #***
 #FUNCAO CAULE
 #-Esta funcao recebe a coluna de uma tabela
 #-Retorna a quantidade de vezes que cada variavel aparece na tabela
 quantidade_para_cada_observacao <- function(coluna, valoresPadrao = NULL) { # coluna = n
   
   if(!is.null(valoresPadrao)) {
     coluna <- subset(coluna, coluna %in% valoresPadrao)
   }
   
   observacoes <- as.factor(coluna) #complexidade n
   retorno <- data.frame(table(observacoes)) #complexidade 2n
   write.xlsx(retorno, "quantidade_para_cada_observacao.xlsx")
   return(retorno)
   
 } #complexidade n(chao de 2n)
 #***
 #Funcoes irmas
 # source("biblioteca/funcionais.R")
 
 #***
 #OPORTUNIDADE
 
 #-Esta funcao recebe duas colunas de uma mesma tabela referente a datas que devem estar no formato %aaaa%mm%dd (ano, mes e dia)
 #-Recebe uma lista de valores padrao e uma coluna que servira para filtrar de acordo com determinado valor padrao.
 #-retorna a media do intervalo de tempo entre as observacoes de uma mesma linha das colunas de datas filtradas de acordo com o escolhido.
 duracao_em_dias <- function(coluna_data_x = NULL, coluna_data_y = NULL, mediana = FALSE
                             ,quantidade_dias_referencia = NULL, valoresPadrao = NULL
                             ,variavel_identificador = NULL, formato = NULL) { # coluna_data_x = n = coluna_data_y, valoresPadrao = m
   
   
   if(xor(is.null(valoresPadrao), is.null(variavel_identificador))) {
     
     return("valoresPadrao ou variavel de valoresPadrao nao indicados")
     
   } else if(is.null(coluna_data_x) || is.null(coluna_data_y)) {
     
     return("As duas colunas devem ser preenchidas")
     
   } else{
     
     coluna_data_x <- valores_em_branco_para_NA(variavel = coluna_data_x) #complexidade n(chao de 2n)
     coluna_data_y <- valores_em_branco_para_NA(variavel = coluna_data_y) #complexidade n(chao de 2n)
     
   }
   
   if(!is.null(formato)) {
     
     coluna_data_x <- as.Date(coluna_data_x, format = formato) #complexidade n>>
     coluna_data_y <- as.Date(coluna_data_y, format = formato) #complexidade n>>
     
   } else {
     
     coluna_data_x <- as.Date(coluna_data_x) #complexidade n>>
     coluna_data_y <- as.Date(coluna_data_y) #complexidade n>>
     
   }
   
   #equivalente ao "difftime" funcao do R.
   diferenca_de_dias <- coluna_data_x - coluna_data_y # complexidade n(chao de 2n)
   
   if(!is.null(quantidade_dias_referencia)) {
     #diferencaInferior <- diferenca_de_dias[diferenca_de_dias < quantidade_dias_referencia] #complexidade 2n
     #diferencaSuperior <- diferenca_de_dias[diferenca_de_dias > quantidade_dias_referencia] #complexidade 2n
     
     
     #Segundo o cliente sao necessarios somentes os calculos referentes as limpezas
     #total_diferenca_inferior <- length(diferencaInferior)
     #total_diferenca_superior <- length(diferencaSuperior)
     
     #Precisa da proporcao?
     #print(paste(total_diferenca_inferior, " registros abaixo de ", quantidade_dias_referencia, " dias",sep = ""))
     #print(paste(total_diferenca_superior, " registros acima de ", quantidade_dias_referencia, " dias",sep = ""))
   }
   
   if(!is.null(valoresPadrao) && !is.null(variavel_identificador)) { 
     
     
     diferenca_de_dias <- subset(diferenca_de_dias, variavel_identificador %in% valoresPadrao) #complexidade nm
     
     total_identificada_inferior <- sum(diferenca_de_dias <= quantidade_dias_referencia, na.rm = TRUE) #complexidade 2n
     total_identificada_superior <- sum(diferenca_de_dias > quantidade_dias_referencia, na.rm = TRUE) #complexidade 2n
 
     
     print(paste(total_identificada_inferior, " registros abaixo de ", quantidade_dias_referencia, " dias"
                 ," para valoresPadrao",sep = ""))
     print(paste(total_identificada_superior, " registros acima de ", quantidade_dias_referencia, " dias"
                 ," para valoresPadrao",sep = ""))
     
   }
   
   media <- mean(diferenca_de_dias, na.rm = TRUE) #complexidade n
   if (mediana == TRUE) {
     mediana <- median(diferenca_de_dias, na.rm = TRUE) #complexidade n
     print(paste("mediana: ",mediana, sep = "")) #complexidade n
   }
   
   print(paste("media: ", media, sep = "")) #complexidade n
   
 } #complexidade nm
 #***
 #CAMINHO
 #biblioteca/qualidade/completitude/completitude.R
 
 
 #-Esta funcao recebe uma coluna referente a alguma variavel de uma tabela
 #-Retorna a proporcao de valores vazios dessa coluna pelo total de linhas da coluna
 #-Variavel deve ser uma coluna de um data frame, ou um vetor
 completitude <- function(variavel) { #variavel = n
   
   # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n(chao de 2n)
   
   linhasVazias <- sum(is.na(variavel), na.rm = TRUE) #complexidade n
   totalLinhas <- length(variavel) #complexidade n
   
   resultado <- (linhasVazias/totalLinhas) * 100 #complexidade n(chao de 2n)
   
   return(resultado)
   
 } #complexidade n(chao de 2n)
 #-Esta funcao recebe uma coluna de uma tabela, e uma lista e valores que representa valores possiveis na coluna
 #-retorna a porcentagem de observacoes da coluna que sao iguais aos valores setados 
 representatividade <- function(variavel, valoresPadrao) {#variavel = n, valoresPadrao = m
   
   # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n(chao de 2n)
   
   variavelFiltrada <- subset(variavel, variavel %in% valoresPadrao) #complexidade nm
   
   quantidadeDado <- length(variavelFiltrada) #complexidade n(chao de 2n)
   total <- length(variavel) #complexidade n
   
   retorno <- (quantidadeDado/total) * 100 #complexidade n
   return(paste("representatividade: ", retorno, "%", sep = ""))
   
 } #complexidade nm
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
 
 
 #-Esta funcao recebe uma coluna de uma tabela, e dois textos referentes a: A chave correspondente a 
 #uma confirmacao do caso verdadeira(chavePositivo), e a chave correspondente a um falso positivo(chave_falso_positivo)
 #-retorna a porcentagem dos valores positivos que eram verdadeiros
 porcentagem_verdadeiro_positivo <- function(variavel, chavePositivo, chave_falso_positivo) {# variavel = n chavePositivo = 1 chave_falso_positivo = 1
   
   # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n(chao de 2n)
   
   quantidadePositivo <- sum(variavel == chavePositivo, na.rm = TRUE) #complexidade n(chao de 2n)
   quantidade_falso_positivo <- sum(variavel == chave_falso_positivo, na.rm = TRUE) #complexidade n(chao de 2n)
   
   retorno <- ((quantidadePositivo)/(quantidadePositivo + quantidade_falso_positivo)) * 100 #complexidade 3
   return(retorno)
   
 } #complexidade n(chao de 2n)
 #Esta funcao funcionara para as duas formulas do eslaide 15
 casos_hospitalizados <- function(variavelX, variavelY) {
   
   
 } #nao entendi bem da forma como esta colocada esta avaliacao no eslaide(ver com Marcela).
 #eslaide 16 nao possui formula definida
 #***
 #CAMINHO
 #transformacao/estratificacaoIdade.R
 
 #-Essa funcao recebe tabela (um data frame), coluna = A coluna de idade que ira ser estratificada (podendo ser uma data de nascimento ou idades inteiras)
 #extratos = A extratificacao escolhida pelo usuario (caso contrario sera usado um modelo padrao), nomeNovaColuna = Pode dar um nome a nova coluna de extratos
 #coluna_eh_data_nascimento = indica se o paramentro coluna eh uma data de nascimento para indicar o que a funcao deve fazer, formatoData = indica o formato da
 #coluna data de nascimento
 #-Se for uma coluna de idades inteiras, apenas cria a extratificacao segundo escolhido. Se for uma coluna de datas, calcula as idades e depois calcula os extratos.
 #-Retorna a mesma tabela com a variavel adicional de extratos agregada ao seu final
  
 #'  extratificacaoIdade
 #'  Constroi extratos para datas (ano de nascimento) ou valores inteiros
 #' 
 #' @param tabela o dataframe em uso
 #' @param coluna a coluna que sera utilizada
 #' @param extratos a definicao dos extratos nos quais havera a divisao
 #' @param nomesExtratos os nomes dos extratos definidos
 #' @param nomeNovaColuna nome da coluna que sera adicionada ao dataframe com os novos extratos
 #' @param coluna_eh_data_nascimento informa se a coluna apresenta informacoes de data de nascimento
 #' @param formatoData informa o formato em que a data esta construida
 #' @example 
 #' Usar com os formatos padroes de idade do ibge 
 #' extratificacaoIdade(tabela = teste, coluna = 'caio')
 #' @export  
 
 
 
 extratificacaoIdade <- function(tabela, coluna, extratos = NULL, nomesExtratos = NULL, nomeNovaColuna = NULL, coluna_eh_data_nascimento = FALSE, formatoData = NULL) {
   
   resultado <- NULL
   
   if(is.null(nomeNovaColuna)) {
     nomeNovaColuna <- "idade_extrato"
   }
   
   if(is.null(extratos)) {
     extratos <- c(0, 1, 5, 10, 20, 30, 40, 50, 60, 70, 150)
   }
   
   if(coluna_eh_data_nascimento) {
     
     if(is.null(formatoData)) {
       coluna <- year(strptime(Sys.Date(), format = "%Y-%m-%d"))-
                     year(strptime(coluna, format = "%Y-%m-%d"))
     
     } else {
       coluna <- year(strptime(Sys.Date(), format = "%Y-%m-%d"))-
                     year(strptime(coluna, format = formatoData))
     }
   }
   
   resultado <- tabela
   
   if(is.null(nomesExtratos)) {
   
     resultado[[nomeNovaColuna]] <- cut(coluna, extratos)
   
   } else {
     
     resultado[[nomeNovaColuna]] <- cut(coluna, extratos, labels = nomesExtratos)
   
   }
   
   return(resultado)
 }