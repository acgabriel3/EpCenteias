
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

