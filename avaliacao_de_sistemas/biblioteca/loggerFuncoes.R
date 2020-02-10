#***
#Acoplando funcoes e log de funcoes, para que todas possam ser salvas

auxC <- completitude_variaveis_de_uma_tabela

completitude_variaveis_de_uma_tabela <- function(tabela, registrar = FALSE, nota = NULL) {


  resultado <- auxC(tabela = tabela, registrar = registrar)

  if(!is.null(log))
    salvarComando(nota)

  return(resultado)

}

auxCB <- completitude

completitude <- function(variavel, nota = NULL) {

  resultado <- auxCB(variavel = variavel)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxCR <- completitudeRelacionada

completitudeRelacionada <- function(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao = NULL, nota = NULL) {

  resultado <- auxCR(tabela, variaveis_de_referencia, variavel_para_avaliacao, valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxCon <- consistencia

consistencia <- function(colunaA = NULL, colunaB = NULL, coluna_adicional_parametro = NULL,
                         data = TRUE, valoresPadrao = NULL, valores_padrao_adicionais = NULL,
                         formato = NULL, nota = NULL) {

  resultado <- auxCon(colunaA = ColunaA, colunaB = ColunaB, coluna_adicional_parametro = coluna_adicional_parametro,
                      data = data, valoresPadrao = valoresPadrao, valores_padrao_adicionais = valores_padrao_adicionais,
                      formato = formato)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxConVarCon <- consistencia_variavel_conjunto

consistencia_variavel_conjunto <- function(tabela, colunaA, colunasB, valores_padrao_A, valores_padrao_B, diferenca = FALSE, nota = NULL) {

  resultado <- auxConVarCon(tabela = tabela, colunaA = colunaA, colunasB = colunasB, valores_padrao_A = valores_padrao_B, valores_padrao_B = valores_padrao_B)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxDurDias <- duracao_em_dias

duracao_em_dias <- function(coluna_data_x = NULL, coluna_data_y = NULL, mediana = FALSE
                            ,quantidade_dias_referencia = NULL, variavelIdentificador = NULL
                            ,valoresPadrao = NULL, formato = NULL, nota = NULL) {

  resultado <- auxDurDias(coluna_data_x = coluna_data_x, coluna_data_y = coluna_data_y, mediana = mediana, quantidade_dias_referencia = quantidade_dias_referencia,
                          variavelIdentificador = variavelIdentificador, valoresPadrao = valoresPadrao, formato = formato)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}


auxExtIda <- extratificacaoIdade

extratificacaoIdade <- function(tabela, coluna, extratos = NULL, nomesExtratos = NULL, nomeNovaColuna = NULL, coluna_eh_data_nascimento = FALSE,
                                formatoData = NULL, nota = NULL) {

 resultado <- auxExtIda(tabela = tabela, coluna = coluna, extratos = extratos, nomesExtratos = nomesExtratos, nomeNovaColuna = nomeNovaColuna,
                        coluna_eh_data_nascimento = coluna_eh_data_nascimento,
                        formatoData = formatoData)

 if(!is.null(nota))
   salvarComando(nota)

 return(resultado)

}

auxPorVer <- porcentagem_verdadeiro_positivo

porcentagem_verdadeiro_positivo <- function(variavel, chavePositivo, chave_falso_positivo, nota = NULL) {

  resultado <- auxPorVer(variavel = variavel, chavePositivo = chavePositivo, chave_falso_positivo = chave_falso_positivo)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxQtObs <- quantidade_de_observacoes

quantidade_de_observacoes <- function(variavel, valoresPadrao = NULL, nota = NULL) {

  resultado <- auxQtObs(variavel = variavel, valoresPadrao = valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxQtPara <- quantidade_para_cada_observacao

quantidade_para_cada_observacao <- function(coluna, valoresPadrao = NULL, nota = NULL) {

  resultado  <- auxQtPara(coluna = coluna, valoresPadrao = valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxRepr <- representatividade

representatividade <- function(variavel, valoresPadrao, nota = NULL) {

  resultado <- auxRepr(variavel = variavel, valoresPadrao = valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxReprCon <- representatividadeCondicionada

representatividadeCondicionada <- function(colunaFiltro, valoresFiltro, colunaAlvo, valoresPadrao, nota = NULL) {

  resultado <- auxReprCon(colunaFiltro = colunaFiltro, valoresFiltro = valoresFiltro, colunaAlvo = colunaAlvo, valoresPadrao = valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxTabVarAva <- tabela_variaveis_avaliacao

tabela_variaveis_avaliacao <- function(variavelBase, variavel, funcao, valoresPadrao = NULL, nota = NULL) {

  resultado <- auxTabVarAva(variavelBase = variavelBase, variavel = variavel, funcao = funcao, valoresPadrao = valoresPadrao)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxVali <- validade

validade <- function(variavel, valoresPadrao, intervalo = FALSE
                     , data = FALSE, formatoData = NULL, nota = NULL) {

  resultado <- auxVali(variavel = variavel, valoresPadrao = valoresPadrao, intervalo = intervalo
                      , data = data, formatoData = formatoData)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxValBrNa <- valores_em_branco_para_NA

valores_em_branco_para_NA <- function(variavel, nota = NULL) {

  resultado <- auxValBrNa(variavel = variavel)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}

auxVerNas <- verificaNas

verificaNas <- function(tabela, nota = NULL){

  resultado <- auxVerNas(tabela = tabela)

  if(!is.null(nota))
    salvarComando(nota)

  return(resultado)

}
