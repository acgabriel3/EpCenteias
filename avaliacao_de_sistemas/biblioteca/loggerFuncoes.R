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
