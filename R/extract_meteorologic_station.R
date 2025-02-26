#' Retorna o dataframe com as informações formatadas sobre as estações meteorológicas
#'
#' Esta função recebe um header, datas de inicio e fim, um indice, datas e os valores de precipitação.
#' @param header cabeçalho do arquivo
#' @param data_inicio data de inicio do cabeçalho
#' @param data_fim data de fim do cabeçalho
#' @param index indice do arquivo
#' @param datas registro histórico das datas das precipitações
#' @param valores registro histórico das precipitações
#' @return O dataframe mesclado com as informações
#' @export
extract_meteorologic_station <- function(header, data_inicio, data_fim, index, datas, valores)
{
  binded = cbind(header,datas, valores, row.names = NULL)
  names(binded)[7] <- 'precipitacao'
  return (binded)
}