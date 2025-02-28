#' Filtra dataframe por lista em arquivo texto
#'
#' Esta função recebe o arquivo, o dataframe e a coluna a ser filtrada
#' @param arquivo arquivo a ser utilizado com dados a serem utilizados como filtro
#' @param dataframe dados caregados no dataframe
#' @param colune_filtro nome da coluna a ser filtrada
#' @return um dataframe filtrado
#' @export
filtrar_por_arquivo <- function(arquivo, df, coluna_filtro) {
  # Ler o arquivo texto e armazenar os valores
  valores_filtro <- readLines(arquivo)
  
  # Filtrar o dataframe com base nos valores lidos
  df_filtrado <- df[df[[coluna_filtro]] %in% valores_filtro, ]
  
  return(df_filtrado)
}