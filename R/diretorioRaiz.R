#' Retorna o diretório raíz do projeto
#'
#' Esta função Não recebe parâmetros e retorna o caminho da pasta raíz do projeto.
#' @return O caminho raíz da aplicação
#' @export
diretorioRaiz <- function() {
  return(paste(dirname(rstudioapi::getSourceEditorContext()$path), "/", sep=""))
}
