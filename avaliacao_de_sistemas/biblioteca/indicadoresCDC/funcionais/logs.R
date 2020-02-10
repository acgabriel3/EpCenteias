
pkg.globals <- new.env()

pkg.globals$Analista <- 'desconhecido'

#' define o autor para os logs
#'
#' @param nome o nome do ator que estah trabalhando com o pacote
#' @export
definirAnalista <- function(nome) {
  pkg.globals$Analista <- nome
}

clearhistory <- function() {
  write("", file="a.txt")
  loadhistory("a.txt")
  file.remove("a.txt")
}

#***
#Pode ser utilizado tambem para a criacao de relatorios automaticos e textos por meio do programa
salvarComando <- function(nota) {

  savehistory()
  clearhistory()
  text <- readLines('.Rhistory')
  file.remove('.Rhistory')
  text <- text[!((text == 'savehistory()') | (text == 'salvarComando()')|(is.na(text)))]
  write(paste("---data:", Sys.time(),"---Analista:", pkg.globals$analista, '---comando:', sep = " "), 'logComand.txt', append = TRUE)
  write(paste( '---nota:' ,as.character(nota), sep = " "), 'logComand.txt', append = TRUE)
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

