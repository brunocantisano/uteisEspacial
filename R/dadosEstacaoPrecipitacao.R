#' Retorna o dataframe com as informações formatadas
#'
#' Esta função recebe um dataframe, datas de inicio e fim e informação se trunca ou não, por 3 dígitos , o resultado da precipitaçpão.
#' @param df dataframe
#' @param data_inicio data de inicio do processamento
#' @param data_fim data de fim do processamento
#' @param trunca informação se trunca ou não o resultado por 3 dígitos
#' @return O dataframe processado
#' @export
dadosEstacaoPrecipitacao <- function(df, data_inicio, data_fim, trunca=FALSE) {
  # renomeia df$codigo para df$cod_estacao
  if(exists("codigo", where = df)) {
    colnames(df)[colnames(df) == "codigo"] <- "cod_estacao"
  }
  df$data <- as.Date(df$data, format = "%Y-%m-%d")
  df <- dplyr::filter(df, between(data, data_inicio, data_fim))
  df <- df[, c(1, 3)]
  df$precipitacao <- gsub(",", ".", df$precipitacao)
  df$precipitacao <- as.numeric(df$precipitacao)
  df <- aggregate(df$precipitacao, by=list(df$cod_estacao), FUN=mean)
  names(df)[1] <- 'cod_estacao'
  names(df)[2] <- 'precipitacao'
  df$precipitacao[is.na(df$precipitacao)] <- 0
  if(trunca){
    df$precipitacao <- round(df$precipitacao, digits=3)
  }
  return(df)
}