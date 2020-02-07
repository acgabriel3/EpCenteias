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
