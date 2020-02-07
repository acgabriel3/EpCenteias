
 #-Esta funcao recebe duas colunas de um data.frame
 #-Pode receber um formato especifico para a data ou valores padrao para calcular a inconsistencia
 #-Calcula a consistencia dos dados da coluna B segundo a coluna A 
 #-Se forem datas, inconsitencia eh entendida como uma data na coluna B anterior a uma data na coluna A
 #-Retorna a porcentagem de valores inconsistentes na coluna B
 
 #' consistencia
 #' @description 
 #' calcula a consistencia de variaveis de fatores e datas
 #' @param colunaA variavel de um dataframe que sera utilizada como referencia
 #' @param colunaB variavel de um dataframe que sera checada
 #' @param coluna_adicional_parametro variavel de um dataframe que sera utilizada para filtrar a tabela
 #' @param data indica se a funcao sera aplicada sobre variaveis de data
 #' @param valoresPadrao o conjunto de fatores a serem filtrados em \code{colunaA}
 #' @param valores_padrao_adicionais o conjunto de fatores a serem filtrador em \code{coluna_adicional_parametro}
 #' @param formato o formato da data utilizado
 #' @return retorna a consistencia com relacao as variaveis determinadas
 #' @examples 
 #' consistencia(colunaA = dengue2013$DT_SORO, colunaB = dengue2013$RESUL_SORO, duas_colunas_data = FALSE)
 #' consistencia(colunaA = dengue2013$RESUL_SORO, colunaB = dengue2013$DT_SORO, duas_colunas_data = FALSE)
 #' consistencia(colunaA = dengue2013$RESUL_SORO, colunaB = dengue2013$DT_SORO, valoresPadrao = c("2", "4"), duas_colunas_data = FALSE)
 #' consistencia(colunaA = dengue2013$RESUL_SORO, colunaB = dengue2013$DT_SORO, coluna_adicional_parametro = dengue2013$NU_ANO, duas_colunas_data = FALSE)
 #' consistencia(colunaA = dengue2013$RESUL_SORO, colunaB = dengue2013$DT_SORO,coluna_adicional_parametro = dengue2013$NU_ANO,
 #'              valoresPadrao = c("2", "4"), valores_padrao_adicionais =  c("2013"),duas_colunas_data = FALSE)
 #' @export
 consistencia <- function(colunaA = NULL, colunaB = NULL, coluna_adicional_parametro = NULL, 
                          data = TRUE, valoresPadrao = NULL
                          , valores_padrao_adicionais = NULL, formato = NULL) { #colunaA = n = colunaB, valoresPadra = m
   
   if(is.null(colunaA) || is.null(colunaB)) {
     return("Entrada invalida, colunaA e colunaB devem ser ambas preenchidas")
   }
   
   # colunaA <- valores_em_branco_para_NA(colunaA) #complexidade n
   # colunaB <- valores_em_branco_para_NA(colunaB) #complexidade n
   
   if(data) {
     
     preenchidos <- !is.na(colunaA)
     colunaA <- colunaA[preenchidos]
     colunaB <- colunaB[preenchidos]
     
     tamanho_coluna_A <- length(colunaA) #complexidade n
     
     if(is.null(formato)) {
       datasA <- as.Date(colunaA) #complexidade n
       #esta coluna sera avaliada
       datasB <- as.Date(colunaB) #complexidade n
     } else {
       datasA <- as.Date(colunaA, format = formato) #complexidade n
       #esta coluna sera avaliada
       datasB <- as.Date(colunaB, format = formato) #complexidade n
     }
     
     valoresInconsistentes <- (datasB - datasA) < 0 #complexidade 2n
     qtdInconsistentes <- sum(valoresInconsistentes, na.rm = TRUE) #complexidade n
     
     proporcao <- (qtdInconsistentes/tamanho_coluna_A) * 100 #complexidade 2n
     
     return(paste("porcentagem de valores inconsistentes na coluna B: ", proporcao, "%", sep = ""))
     
   } else {
     
     if(is.null(valoresPadrao)) {
       
       valoresPreenchidos <- !is.na(colunaA) #complexidade 2n
       dadosInconsistentes <- sum(is.na(colunaB[valoresPreenchidos]), na.rm = TRUE) #complexidade 3n
       proporcao <- (dadosInconsistentes/length(colunaA[valoresPreenchidos])) * 100 #complexidade 4n
       
       return(paste("Proporcao de valores inconsistentes: ", proporcao, "%", sep = ""))
       
     } else {
       
       coluna_filtrada <- subset(colunaB, colunaA %in% valoresPadrao) #complexidade nm
       proporcao <- (sum(is.na(coluna_filtrada), na.rm = TRUE)/length(coluna_filtrada)) * 100 #complexidade 3n
       return(paste("Proporcao de valores inconsistentes: ", proporcao, "%", sep = ""))
       
     }
     
   }
   
   if(!is.null(coluna_adicional_parametro)) {
     coluna_adicional_parametro <- valores_em_branco_para_NA(coluna_adicional_parametro)
     
     if(is.null(valoresPadrao)) {
       
       valoresPreenchidos <- !is.na(colunaA) & !is.na(coluna_adicional_parametro) #xomplexidade 5n
       dadosInconsistentes <- sum(is.na(colunaB[valoresPreenchidos]), na.rm = TRUE) #complexidade 3n
       proporcao <- (dadosInconsistentes/length(colunaA[valoresPreenchidos])) * 100 #complexidade 4n
       
       return(paste("Proporcao de valores inconsistentes: ", proporcao, "%", sep = ""))
       
     } else {
       
       if(is.null(valores_padrao_adicionais)) {
         return("valores_padrao_adicionais deve ser fornecido")
       }
       
       coluna_filtrada <- subset(colunaB, (colunaA %in% valoresPadrao) & 
                                   (coluna_adicional_parametro %in% valores_padrao_adicionais)) #complexidade 2nm + n
       proporcao <- (sum(is.na(coluna_filtrada), na.rm = TRUE)/length(coluna_filtrada)) * 100 #complexidade 3n
       return(paste("Proporcao de valores inconsistentes: ", proporcao, "%", sep = ""))
       
     }
     
   }
   
   return("entrada invalida")
   
 } #complexidade n(chao de 2n) (para duas datas) ou complexidade n(chao de 4n) (para valoresPadrao null e nao data) ou complexidade nm (chao de 2nm+n) (para valoresPadrao setados)

 #Consistencia alterada
 
 #***
 #rever funcao e proposta
 
 #' consistencia_variavel_conjunto
 #' @description 
 #' funcao ainda em desenvolvimento, nao usar
 #' @param tabela um dataframe sobre o qual serao realizadas as operacoes
 #' @param colunaA a coluna de referencia
 #' @param colunasB as colunas sobre as quais sera calculada a consitencia com relacao a \code{colunaA}
 #' @param valores_padrao_A um conjunto de fatores de \code{colunaA} que serao considerados
 #' @param valores_padrao_B um conjunto e conjunto de faotes para cada \code{colunasB}
 #' @param diferenca ainda em implementacao
 #' @return retorna a consistencia com relacao a cada variavel
 #' @export
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
 
 #' aplicacao_todas_as_colunas
 #' @description 
 #' funcao em desenvolvimento que aplica uma funcao a todas as colunas de uma tabela de acordo com uma coluna referencia
 #' @param colunaBase uma variavel de fatores de um dataframe 
 #' @param tabela um dataframe sobre o qual serao realizadas as operacoes
 #' @param funcao a funcao que sera aplicada sobre as colunas de \code{tabela}
 #' @param valoresPadrao o conjunto de valores que sera considerado para a aplicacao da funcao
 #' @return retorna os resultados da aplicacao da funcao em cada coluna
 #' @export
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
 
 #' tabela_variaveis_avaliacao
 #' @param variavelBase variavel referencia de um dataframe
 #' @param variavel variavel de um dataframe sobre a qual serao realizadas as operacoes
 #' @param funcao funcao que sera aplicada sobre a \code{variavel}
 #' @param valoresPadrao o conjunto de fatores considerado nos quais serao calculadas a operacao
 #' @return retorna o resultado e registra em tabela xlsx da \code{funcao} sobre a \code{variavel} aplicado apenas as linhas que possuem os \code{valoresPadrao} na \code{variavelBase}
 #' @examples 
 #' tabela_variaveis_avaliacao(dengue2013$CS_SEXO, dengue2013$RESUL_SORO, representatividade, valoresPadrao = c("2"))
 #' @export
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

 #Objetivo: Criar uma funcao para aplicar outras funcoes a todas as colunas de uma tabela. 
 
 #***
 #Padronizar entradas em todas as funcoes da biblioteca.
 #***
 #Seria necessario colocar todas as possibilidades possiveis de entrada, talvez seja possivel 
 #modularizar este processo (pensar acerca)
 #***
 #Posso utilizar aqui um swich
 
 #' aplicador_tabelas
 #' funcao ainda em desenvolvimento que aplica uma funcao a todas as variaveis de uma tabela
 #' @param tabela o dataframe sobre o qual sera realizada a operacao
 #' @param funcao em aplicacao
 #' @param funcao2 em aplicacao
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
 #biblioteca/funcionais/aplicacao/filtro_mes_servico.R
 
 library(lubridate)

 #- Esta funcao recebe de uma a tres tabelas, respectivamente com: 1 a 3 nomes de colunas
 #com informacoes, 1 a 3 nomes de colunas acerca de data, e 1 a 3 colunas acerca de valores 
 #categoricos. Recebe valoresPadrao, casos, meses, e anos.
 #- Esta funcao filtra segundo os anos e meses dados, as colunas de informacao, segundo
 #as categorias das colunas categoricas, e aplica uma funcao qualquer a estes dados. 
 #(deve-se preencher corretamente a funcao, de acordo com a funcao que sera aplicada (indicador))
 #- Retorna uma tabela com os resultados da funcao aplicada para cada categoria e data escolhida.
 
 #' filtro_mes_variavel
 #' funcao experimental que filtra por mes e por variavel algumas tabelas para a construcao de um indicador epidemiologico
 #' @export
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

 #'  variavel de autor
 #'  @description 
 #'  seta o autor que esta escrevendo no momento
 #'  @export
 autor <- 'Gabriel Alves Castro'

 clearhistory <- function() {
   write("", file="a.txt")
   loadhistory("a.txt")
   file.remove("a.txt")
 }

 #***
 #Pode ser utilizado tambem para a criacao de relatorios automaticos e textos por meio do programa
 salvarComando <- function() {
   
   savehistory()
   clearhistory()
   text <- readLines('.Rhistory')
   file.remove('.Rhistory')
   text <- text[!(text == 'savehistory()' | text == 'salvarComando()')]
   write(paste("---data:", Sys.time(),"---autor:", autor, '---comando:', sep = " "), 'logComand.txt', append = TRUE)
   write(text, 'logComand.txt', append = TRUE)
 
 }

 # clearhistory()
 # 
 # completitude_variaveis_de_uma_tabela(dengue2013) #precisa registrar um log das operacoes ---> Isto seria em RMarkdDown ou um txt comum? 
 # saveCommand()
 # 
 # a <- function() {
 #   print('oi')
 # }
 # saveCommand()
 

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

 #***
 #CAMINHO
 #biblioteca/funcionais/aplicacao/aplicacaoBase/quantidade_para_cada_observacao.R
 
 library(xlsx)

 #***
 #FUNCAO CAULE
 #-Esta funcao recebe a coluna de uma tabela
 #-Retorna a quantidade de vezes que cada variavel aparece na tabela
 
 #' quantidade_para_cada_observacao
 #' @description 
 #' calcula com table do dplyr
 #' @param variavel recebe um vetor de fatores
 #' @param valoresPadrao o conjunto de fatores que sera considerado para realizar a operacao
 #' @return retorna a quantidade de cada fator de \code{valoresPadrao} em \code{variavel}
 #' @examples 
 #' quantidade_para_cada_observacao(variavel = dengue2013$MUNICIPIO)
 #' quantidade_para_cada_observacao(variavel = dengue2013$RESUL_SORO, valoresPadrao = c("2", "4"))
 #' @export
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
 #Prototipo para o relatorio
 
 #' @export
 paragraph <- 
 'estrutura bla bla
 #1 
 olha execute isso'

 clearhistory <- function() {
   write("", file="a.txt")
   loadhistory("a.txt")
   file.remove("a.txt")
 }

 #***
 #Pode ser utilizado tambem para a criacao de relatorios automaticos e textos por meio do programa
 salvar_comando_rel <- function() {
   
   if(sum(dir() == 'relatorio.rmd') == 0) {
     write( '---
 title: "relatorio"
 output: html_notebook
 ---
 ', 'relatorio.rmd', append = TRUE)
   }
   
   
   savehistory()
   clearhistory()
   text <- readLines('.Rhistory')
   file.remove('.Rhistory')
   text <- text[!(text == 'savehistory()' | text == 'salvar_comando_rel()')]
   write(paragraph, 'relatorio.rmd', append = TRUE)
   write('```{r}', 'relatorio.rmd', append = TRUE)
   write(text, 'relatorio.rmd', append = TRUE)
   write('```', 'relatorio.rmd', append = TRUE)
 
 }

 # clearhistory()
 # print('Olah mundo dos relatorios')
 # saveCommand()

 #' verificaNas
 #' @param tabela um dataframe sobre o qual serao realizadas as operacoes
 #' @return retorna um hash com o nome de cada coluna da tabela e os valores em branco contidos nessa coluna
 #' @examples 
 #' verificaNas(dengue2013)
 #' @export
 verificaNas <- function(tabela){
   
   resultado <- c(NULL)
   
   for(i in 1:length(tabela)) {
     
     '%UNE%' <- function(x,y) paste(x, y) 
     
     print(paste("Quantidade Nas coluna : ", colnames(tabela)[i], " " ,sep = ""))
     
     nomeVariavel <- colnames(tabela)[i]
     nas <- sum(is.na(tabela[[i]]), na.rm = TRUE)
     
     resultado <- 
       
       eval(parse(text =
                    "c(resultado," %UNE%
                    nomeVariavel %UNE%
                    "=" %UNE%
                    "nas" %UNE%
                    ")"
         
         
       ))
     
     #***
     #Como fazer isso sem metaprog? 
     
     print(nas)
     
   }
   
   return(resultado)
   
 }

 #***
 #Funcoes irmas
 # source("biblioteca/funcionais.R")
 
 #***
 #OPORTUNIDADE
 
 #-Esta funcao recebe duas colunas de uma mesma tabela referente a datas que devem estar no formato %aaaa%mm%dd (ano, mes e dia)
 #-Recebe uma lista de valores padrao e uma coluna que servira para filtrar de acordo com determinado valor padrao.
 #-retorna a media do intervalo de tempo entre as observacoes de uma mesma linha das colunas de datas filtradas de acordo com o escolhido.
 
 #' duracao_em_dias
 #' @description 
 #' funcao ainda em desenvolvimento, tera como objetivo o calculo da duracao media em dias de dois eventos, recebendo a data inicio x e a data final x do evento
 #' e outras informacoes
 #' @param coluna_data_x vetor de um dataframe com a data final do evento
 #' @param coluna_data_y vetor de um dataframe com a data de inicio do evento
 #' @param mediana indica se a mediana devera ser calculada
 #' @param quantidade_dias_referencia em implementacao
 #' @param variavelIdentificador uma variavel qualitativa do datafram sobre o qual serao determinados os eventos de interesse (quando preenchido)
 #' @param valoresPadrao os casos que devem ser considerados para \code{variavelIdentificador}
 #' @param formato o formato de data que deve ser utilizado
 #' @return prints com sumarios da distribuicao da duracao do evento, media e mediana da duracao do evento
 #' @examples 
 #' duracao_em_dias(dengue2013$DT_NOTIFIC, dengue2013$DT_SIN_PRI, mediana = TRUE
 #'                ,valoresPadrao = c("2", "4"), variavel_identificador = dengue_2013$RESUL_SORO)
 #' @export
 duracao_em_dias <- function(coluna_data_x = NULL, coluna_data_y = NULL, mediana = FALSE
                             ,quantidade_dias_referencia = NULL, variavelIdentificador = NULL
                             ,valoresPadrao = NULL, formato = NULL) { # coluna_data_x = n = coluna_data_y, valoresPadrao = m
   
   
   if(xor(is.null(valoresPadrao), is.null(variavelIdentificador))) {
     
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
   
   if(!is.null(valoresPadrao) && !is.null(variavelIdentificador)) { 
     
     
     diferenca_de_dias <- subset(diferenca_de_dias, variavelIdentificador %in% valoresPadrao) #complexidade nm
     
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
 
 #' completitude
 #' @param variavel um vetor, ou variavel (coluna) de um dataframe sobre a qual a operacao sera realizada
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

 #' completitude_relacionada
 #' @description 
 #' calcula a completitude de uma variavel de acordo com um conjunto de fatores presentes em um conjunto de variaveis de referencia
 #' @param tabela o dataframe em que os calculos serao realizados
 #' @param variaveis_de_referencia o conjunto de variaveis utilizadas como referencia, quando combinadas devem indicar uma situacao
 #' @param variavel_para_avaliacao a variavel sobre a qual sera calculada a completitude
 #' @param valoresPadrao um conjunto de vetores, com os fatores de cada uma das variaveis de referencia, respectivamente
 #' @examples 
 #' completitude_relacionada(dengue2013, ("CS_SEXO"), c("CS_GESTANT"), c(c("F")))
 #' @return Retorna a completitude da \code{variavel_para_avaliacao} contendo apenas as linhas que possuiam determinados conjuntos de fatores
 #' nas \code{variaveis_de_referencia} de acordo com os \code{valoresPadrao}
 #' @export
 completitudeRelacionada <- function(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao = NULL
                                      
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

 #***
 #Melhorar o nome da funcao logo abaixo
 
 #-Esta funcao recebe uma coluna referente a alguma variavel de uma tabela, uma lista dos valores padrao da coluna
 #e duas variaveis logicas indicando se eh uma avaliacao de intervalo ou de data
 #-Retorna a proporcao de valores que nao estao de acordo com os valores padroes, e indica quais sao estes.
 #-Variavel deve ser uma coluna de uma data frame, ou um vetor
 #-ValoresPadrao deve ser uma coluna de um data frame, ou um vetor(mesmo que possua apenas um valor)
 #-ValoresPadrao deve apresentar o menor valor na primeira posicao, e o maior na segunda caso intervalo seja TRUE
 #***
 #como tratar os valores nulos? 
 #***
 #Refatorar funcao para melhorar a usabilidade
 #estah eh a validade?
 
 #' validade
 #' @description 
 #' Calcula a validade para um vetor
 #' @param variavel um vetor, normalmente proveniente de uma variavel de um dataframe, ao qual serao realizadas as operacoes
 #' @param valoresPadrao indica todos os valores que a \code{variavel} de fatores sobre a qual sera realizada a operacao deve conter
 #' @param data indica se a \code{variavel} de fatores possui o formato de data
 #' @param formatoData indica qual formato de data a funcao deve considerar, so pode ser utilizada se \code{data} for verdadeiro
 #' @return retorna a validade de \code{variavel} em character
 #' @examples 
 #' validade(variavel = dengue2013$RESUL_SORO, valoresPadrao = c("1", "2", "3","4"))
 #' validade(variavel = dengue2013$DT_NOTIFIC, valoresPadrao = c("2013/01/01","2013/12/01"), data = TRUE)
 #' -O valor a esquerda em valores padrao eh o limite inferior (por exemplo se quiser incluir 1000, coloque 999)
 #' -O valor a direita em valores padrao eh o limite superior (por exemplo, se quiser incluir 3000, coloque 3001)
 #' validade(dengue_2013$NU_IDADE_N, valoresPadrao = c("1000", "3000"), intervalo = TRUE) 
 #' @export
 validade <- function(variavel, valoresPadrao, intervalo = FALSE
                             , data = FALSE, formatoData = NULL) { #variavel = n, valoresPadrao = m
   
   if((data == TRUE) && (intervalo == TRUE)) {
     
     return("data e intervalo nao podem ser simultaneamente verdadeiros")
     
   } else {
     
     variavel <- as.character(variavel) #complexidade n
     # variavel <- valores_em_branco_para_NA(variavel = variavel) #complexidade n
     totalVariavel <- length(variavel) #complexidade n
     
   }
   
   if((data == FALSE) && (intervalo == FALSE)) {
     
     totalCorretos <- sum(is.na(variavel))
     
     variavel <- variavel[!is.na(variavel)] #complexidade n
     
     #***
     #as funcoes apply podem ser facilmente empregadas aqui?
     for(i in 1:length(valoresPadrao)) { 
       
       totalCorretos <- totalCorretos + sum(variavel == valoresPadrao[i], na.rm = TRUE) #complexidade n
       
     } #complexidade m.n, com m << n 
     
     #Encontra as posicoes em que existem variaveis que nao estão na lista de variaveis padrao
     variavelIncongruentes <- c(variavel[is.na(match(variavel, valoresPadrao))]) #complexidade n
     variavelIncongruentes <- variavelIncongruentes[!is.na(variavelIncongruentes)]
     
     if(length(variavelIncongruentes) == 0) {
       variavelIncongruentes <- "vazia"
     }
     
     proporcao <- 100 - ((totalCorretos/totalVariavel) * 100) #complexidade n
     
     return(paste(proporcao, "% ", "de valores incongruentes ||| "
                  , "lista de valores incongruentes: ", toString(levels(as.factor(variavelIncongruentes))), sep = "")) #complexidade x << n
     
     
   }
   
   if(data == TRUE) {
     #checa se ha alguma observacao que nao seja uma data
     
     #Caso tenha sido imposto um formato, o utiliza, caso contrario realiza uma chamada padrao
     if(is.null(formatoData)) { 
       
       totalCorretos <- sum(!is.na(as.Date(variavel))) #complexidade 2n
       
     } else {
       
       totalCorretos <- sum(!is.na(as.Date(variavel, format = formatoData))) #complexidade 2n
       
     }
     
     proporcao <- 100 - ((totalCorretos/totalVariavel) * 100) #complexidade n
     return(paste(proporcao, "% ", "de valores incongruentes", sep = ""))
     
   }
   
   if(intervalo == TRUE) {
     
     if(length(valoresPadrao) != 2) {
       return("Intervalo invalido")
     }
     
     #checa se ha alguma informacao fora do intervalo definido
     variavel <- as.numeric(variavel)
     valoresPadrao <- as.numeric(valoresPadrao)
     totalCorretos <- sum((variavel < valoresPadrao[2]) & (variavel > valoresPadrao[1]), na.rm = TRUE) #complexidade 3n
     proporcao <- 100 - ((totalCorretos/totalVariavel) * 100) #complexidade n
     return(paste(proporcao, "% ", "de valores incongruentes", sep = ""))
     
   }
   
   return("erro, entrada incapaz de ser executada")
   
 } #complexidade n.m

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

 #-Esta funcao recebe duas colunas de uma tabela, que devem possuir tamanho igual. Tambem recebe dois vetores de valores possiveis na tabela.
 #-Retorna a porcentagem dos valores de valoresPadrao encontrados na colunaAlvo, que possuem tambem os valoresFiltro na colunaFiltro
 #' representatividadeCondicionada
 #' @description 
 #' funcao que calcula a representavidade de uma variavel, de acordo com uma variavel referencia de fatores (para um conjunto de fatores)
 #' @param colunaFiltro recebe a coluna de um dataframe que registra os fatores
 #' @param valoresFiltro recebe os fatores da variavel \code{colunaFiltro} que serao utilizados
 #' @param colunaAlvo recebe a coluna do mesmo dataframe que \code{colunaFiltro} do qual sera calculada a representatividade
 #' @param valoresPadrao o conjunto de fatores que irao ser calculados perante o total de observacoes
 #' @return Retorna a porcentagem dos valores de \code{valoresPadrao} encontrados na \code{colunaAlvo}, que possuem tambem os \code{valoresFiltro} na \code{colunaFiltro}
 #' @examples
 #' representatividadeCondicionada(colunaFiltro = dengue2013$RESUL_SORO, 
 #'                                valoresFiltro = c("2","4"), 
 #'                                colunaAlvo = dengue2013$NU_ANO, 
 #'                                valoresPadrao = c("2013")
 #'                                )
 #' @export
 representatividadeCondicionada <- function(colunaFiltro, valoresFiltro, colunaAlvo, valoresPadrao) { #colunaFiltro = n = colunaAlvo, valoresFiltro = t, valoresPadrao = j
   
   if(is.null(colunaFiltro) || is.null(valoresFiltro) || is.null(colunaAlvo) || is.null(valoresPadrao)) {
     return("valores passados a funcao sao invalidos")
   } #complexidade 2n + t + j
   
   # colunaFiltro <- valores_em_branco_para_NA(variavel = colunaFiltro) #complexidade n
   # colunaAlvo <- valores_em_branco_para_NA(variavel = colunaAlvo) #complexidade n
   
   colunaFiltrada <- subset(colunaAlvo, colunaFiltro %in% valoresFiltro) #complexidade nt
   
   representatividade(colunaFiltrada, valoresPadrao) #complexidade nj
   
 } #complexidade nt + nj

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
 
 #' porcentagem_verdadeiro_positivo
 #' @param variavel a variavel de um dataframe com os fatores investigados
 #' @param chavePositvo os fatores indicando quais sao os fatores verdadeiramente positivos
 #' @param chave_falso_positivo os fatores indicando quais sao os fatores que representam casos aferidos erroneamente como positivos
 #' @examples
 #' porcentagem_verdadeiro_positivo(dengue2013$RESUL_SORO, "2", "4")
 #' @return  retorna a porcentagem dos valores positivos que eram verdadeiros
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
 
 #' extratificacaoIdade
 #' @description
 #' Constroi extratos para datas (ano de nascimento) ou valores inteiros
 #' @param tabela o dataframe ao qual serah adicionada a nova coluna
 #' @param coluna a coluna que sera extratificada (podendo ser numeric ou datas de nascimento)
 #' @param extratos a definicao dos extratos nos quais havera a divisao
 #' @param nomesExtratos os nomes dos extratos definidos
 #' @param nomeNovaColuna nome da coluna que sera adicionada ao dataframe com os novos extratos
 #' @param coluna_eh_data_nascimento informa se a coluna apresenta informacoes de data de nascimento
 #' @param formatoData informa o formato em que a data esta respresentada. Obs: O parametro só pode ser preenchido se coluna_eh_data_nascimento for verdadeiro.
 #' @return O dataframe \code{tabela} com uma nova variavel representando a \code{coluna} extratificada com os \code{extratos} definidos, ou padrao.
 #' @examples
 #' extratificacaoIdade(tabela = dengue2013,
 #'                     coluna = dengue2013$DT_NASC,
 #'                     coluna_eh_data_nascimento = TRUE,
 #'                     nomesExtratos = c("<1", "1-4", "5-9", "10-19", "20-29", "30-29", "40-49", "50-59", "60-69", "70+")
 #')
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

 #***
 #Acoplando funcoes e log de funcoes
 
 auxC <- completitude_variaveis_de_uma_tabela

 completitude_variaveis_de_uma_tabela <- function(tabela, registrar = FALSE, log = TRUE) {
   
   
   resultado <- auxC(tabela = tabela, registrar = registrar)
   
   if(log)
     salvarComando()
       
   return(resultado)
   
 }

 auxCB <- completitude

 completitude <- function(variavel, log = TRUE) {
   
   resultado <- auxCB(variavel = variavel)
   
   if(log)
     salvarComando()
   
   return(resultado)
   
 }

 auxCR <- completitudeRelacionada

 completitudeRelacionada <- function(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao = NULL, log = TRUE) {
   
   resultado <- auxCR(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao)
   
   if(log)
     salvarComando()
   
   return(resultado)
   
 }
