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