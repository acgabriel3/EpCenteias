

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
