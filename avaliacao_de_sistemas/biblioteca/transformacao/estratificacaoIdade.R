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
#' @param tabela o dataframe ao qual serah adicionada a nova coluna
#' @param coluna a coluna que sera extratificada (podendo ser numeric ou datas de nascimento)
#' @param extratos a definicao dos extratos nos quais havera a divisao
#' @param nomesExtratos os nomes dos extratos definidos
#' @param nomeNovaColuna nome da coluna que sera adicionada ao dataframe com os novos extratos
#' @param coluna_eh_data_nascimento informa se a coluna apresenta informacoes de data de nascimento
#' @param formatoData informa o formato em que a data está respresentada. Obs: O parâmetro só pode ser preenchido se coluna_eh_data_nascimento for verdadeiro. 
#' @return O dataframe \code{tabela} com uma nova variável representando a \code{coluna} extratificada com os \code{extratos} definidos, ou padrão.
#' @example 
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


