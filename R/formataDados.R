#' Retorna o dataframe com as informações formatadas sobre as estações meteorológicas
#'
#' Esta função recebe um header, datas de inicio e fim, um indice, datas e os valores de precipitação.
#' @param dados_sc dados do órgão analisado
#' @param mes_escolhido mês escolhido para análise
#' @param ano_escolhido ano escolhido para análise
#' @return O dataframe filtrado com as informações
#' @export
formataDados <- function(dados_sc, mes_escolhido, ano_escolhido) {
  ano_2digitos <- str_sub(ano_escolhido,-2,-1)
  df <- dplyr::filter(dados_sc, data %in%
                        c(paste(mes_escolhido,"/",ano_2digitos, sep="")))
 
  df_novo = data.frame("cod_estacao", "data_temp", "precipitacao")
  for (row in 1:nrow(df)) {
    cod_estacao <- df[row, "cod_estacao"]
    for(dia in 1:31) {
      df_novo[nrow(df_novo) + 1,] <- c(cod_estacao, paste(ano_escolhido, '-', mes_escolhido, '-', str_pad(dia, 2, pad = "0"),  sep=""), df[row, as.character(dia)])  
    }
  }
  names(df_novo) <- as.matrix(df_novo[1, ])
  # substituindo NA por zero
  df_novo$precipitacao[is.na(df_novo$precipitacao)] <- 0
  df_novo$data <- as.Date(df_novo$data_temp, format = "%Y-%b-%d")
  df_novo = df_novo[-1,]
  rownames(df_novo) <- NULL
  df_novo <- df_novo[, c(1, 4, 3)]
  return(df_novo)
}