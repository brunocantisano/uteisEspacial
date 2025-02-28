#' Salva dados agrupados por mes
#'
#' Esta função recebe o mes e o ano e agrupa por mês e ano, gerando uma html para navegação
#' @param mes numero do mes a ser utilizado como agrupador
#' @param ano ano a ser utilizado como agrupador
#' @export
medicoes <- function(mes, ano) {
  if(mes < 1 || mes > 12){
    return(FALSE) 
  }
  nome_mes <- format(as.Date(paste0(ano, "-", mes, "-01")), "%B")
  depara_estacoes <- read.csv(file = paste(diretorioRaiz(), "dados/auxiliares/depara_estacoes.csv", sep=""), sep=";")
  load(paste(diretorioRaiz(), "dados/meses/oeste_sc_precipitacao_",nome_mes, opt$ano, ".RData", sep = ""))
  
  dados <- merge(x = dados_sc, y = depara_estacoes, by = "cod_estacao", all = TRUE)
  dados <- na.omit(dados)
  
  shp_oeste_municipios_sc <- st_read(paste(diretorioRaiz(), "shapefiles/SC_Oeste_Municipios/SC_Oeste_Municipios.shp", sep = ""))
  
  # Observando a base de dados carregada
  dados %>% 
    kable() %>%
    kable_styling(bootstrap_options = "striped", 
                  full_width = TRUE, 
                  font_size = 16)
  
  # Se tenho duas ou mais estacoes no mesmo municipio, faço uma média de precipitação das estações 
  # encontradas nos municípios
  dados_agrupados <- dados[ ,list(precipitacao=mean(precipitacao)), by=c("CD_MUN","data")]
  dados_distintos <- dados %>% distinct(CD_MUN, .keep_all = TRUE)
  
  # Para combinar os dados do objeto dados_sp com a base de dados de nosso 
  # shapefile, podemos utilizar a função merge():
  shp_dados_oeste_municipios_sc <- merge(x = shp_oeste_municipios_sc,
                                         y = dados_agrupados,
                                         by.x = "code_mn",
                                         by.y = "CD_MUN")
  
  shp_dados_oeste_municipios_sc %>% 
    kable() %>%
    kable_styling(bootstrap_options = "striped", 
                  full_width = TRUE, 
                  font_size = 12)
  
  # Salvando nosso shapefile:
  # Defina o caminho completo para o arquivo
  caminho_completo <- paste(diretorioRaiz(), "shapefiles",nome_mes, sep = "/")
  
  # Criar o diretório, se ele não existir
  if (!dir.exists(caminho_completo)) {
    dir.create(caminho_completo, recursive = TRUE)  # 'recursive' cria diretórios intermediários
  }
  
  st_write(shp_dados_oeste_municipios_sc, 
           paste(diretorioRaiz(), "shapefiles/",nome_mes,"/shapefile_",nome_mes, opt$ano, ".shp", sep = ""), 
           delete_dsn = TRUE)
  
  # VISUALIZAÇÃO DE DADOS ESPACIAIS
  #grafico <- shp_dados_oeste_municipios_sc %>% 
  #  ggplot() +
  #  geom_histogram(aes(x = precipitacao),
  #                 fill = "deepskyblue4",
  #                 color = "white") +
  #  labs(x = "Precipitação Mensal",
  #       y = "Frequência") +
  #  theme_bw()
  
  # Salvar como imagem
  #ggsave(paste(diretorioRaiz(), "/", "dados/html/histograma_", nome_mes, "_", ano, ".png", sep=""), plot = grafico, width = 8, height = 6, dpi = 300)
  
  bbox_new <- st_bbox(shp_dados_oeste_municipios_sc) # current bounding box
  
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  # bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
  # bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top
  
  bbox_new <- bbox_new %>%  # take the bounding box ...
    st_as_sfc() # ... and make it a sf polygon
  
  nome_estado = shp_dados_oeste_municipios_sc$nam_stt[1]
  # bordas aos polígonos
  img <- tm_shape(shp = shp_dados_oeste_municipios_sc, bbox = bbox_new) + 
    tm_fill(col = "precipitacao", 
            style = "quantile", 
            n = 4, 
            palette = "viridis", 
            legend.hist = FALSE) +
    tm_layout(legend.position = c("right", "bottom"),
              legend.text.size = 0.9,
              legend.title.size = 0.9,
              legend.hist.size = 0.9,
              legend.hist.height = 0.9,
              legend.hist.width = 0.9,
              frame = F,
              main.title = paste("Distribuição das chuvas em ", nome_mes, "/", ano, " nos municípios de", nome_estado)) +
    tm_borders(alpha = 0.5) 
  
  # save image
  tmap_save(img, paste(diretorioRaiz(), "/", "dados/html/medicao_", nome_mes, "_", ano, ".html", sep=""))
  
  return(TRUE)
}