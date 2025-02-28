#' Salva dados agrupados dia, mes e ano
#'
#' Esta função recebe o dataframe, o mes e o ano e agrupa por dia, mes e ano
#' @param dataframe dados caregados no dataframe
#' @param mes numero do mes a ser utilizado como agrupador
#' @param ano ano a ser utilizado como agrupador
#' @export
medicoes_dias_mes <- function(df, mes, ano) {
  if(mes < 1 || mes > 12){
    return(FALSE) 
  }
  nome_mes <- format(as.Date(paste0(ano, "-", mes, "-01")), "%B")
  mes_abreviado <- substr(nome_mes, start = 1, stop = 3)
  dados_sc <- dplyr::filter(df, data==paste(mes_abreviado, " ", opt$ano, sep=""))
  
  # Defina o caminho completo para o arquivo
  caminho_completo <- paste(diretorioRaiz(), "dados/meses", sep = "/")
  
  # Criar o diretório, se ele não existir
  if (!dir.exists(caminho_completo)) {
    dir.create(caminho_completo, recursive = TRUE)  # 'recursive' cria diretórios intermediários
  }
  save(dados_sc, file = paste(diretorioRaiz(), "/", "dados/meses/oeste_sc_precipitacao_", nome_mes, ano, ".RData", sep=""))
}