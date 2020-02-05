
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
