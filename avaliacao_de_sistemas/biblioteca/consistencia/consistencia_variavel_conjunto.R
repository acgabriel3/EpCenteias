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
