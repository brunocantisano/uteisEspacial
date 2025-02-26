#' Retorna os dados do inmet formatados a partir do que foi lido no arquivo csv
#'
#' Esta função recebe um arquivo csv e retorna o dados carregados a partir da 10ª linha do arquivo csv
#' @param file  arquivo csv do inmet
#' @param separador  caracter separador do csv
#' @param nomes_colunas  nomes das colunas a serem utilizadas
#' @return dados do inmet formatados a partir do que foi lido no arquivo csv
#' @export
readFileinMetCsv <- function(file, separador, nomes_colunas){
  estacao <- read.csv(file,
                      header=TRUE,
                      skip=10,
                      sep=separador,
                      col.names=nomes_colunas)
  return(estacao)
}