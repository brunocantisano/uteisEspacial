medicoes_temporada <- function(df, estacao, dados, ano) {
  #Verão: de 21/12 a 31/12 do ano anterior a ser estudado
  #Outono: de 21 de março a 21 de junho do ano a ser estudado
  #Inverno: de 21 de junho a 22 de setembro do ano a ser estudado
  #Primavera: de 22 de setembro a 21 de dezembro do ano a ser estudado
  
  dia_inicio=0
  dia_fim=0
  mes_inicio=0
  mes_fim=0  
  ano_aux=ano
  if(estacao=="outono"){
    dia_inicio=21
    dia_fim=21
    mes_inicio=3
    mes_fim=mes_inicio+3
  } else if(estacao=="inverno"){
    dia_inicio=21
    dia_fim=22
    mes_inicio=6
    mes_fim=mes_inicio+3
  } else if(estacao=="primavera"){
    dia_inicio=22
    dia_fim=21
    mes_inicio=9
    mes_fim=mes_inicio+3
  } else if(estacao=="verao"){
    dia_inicio=21
    dia_fim=31
    mes_inicio=12
    mes_fim=12
    ano_aux=as.character((as.numeric(ano)-1))
  } else {
    return(FALSE)
  }  
  depara_estacoes <- read.csv(file = paste(diretorioRaiz(), "dados/auxiliares/depara_estacoes.csv",sep=""), sep=";")
  load(file = paste(diretorioRaiz(), "/", "dados/ana/ana_",estacao,"_sc_", opt$ano, ".RData", sep=""))
  inicio = paste(ano_aux, "-", str_pad(mes_inicio, width = 2, pad = "0"), "-",dia_inicio,sep="")
  fim = paste(ano_aux, "-",str_pad(mes_fim, width = 2, pad = "0"),"-",dia_fim,sep="")
  
  epagri_inmet <- dplyr::filter(df, data >= inicio & data <= fim)
  # agrupando para análise de volume mensal de chuva e descobre a média de precipitação por mês
  epagri_inmet$data <- as.yearmon(epagri_inmet$data, "%Y-%m")
  
  #média por mês
  epagri_inmet <- epagri_inmet[ ,list(precipitacao=mean(precipitacao)), by=c("cod_estacao","data")]
  epagri_inmet$estacao <- estacao
  epagri_inmet <- epagri_inmet[, c(1, 4, 3)]
  
  epagri_inmet_ana <- rbind(epagri_inmet, dados, fill=TRUE)
  
  dados_estacao <- epagri_inmet_ana[ ,list(precipitacao=mean(precipitacao)), by=c("cod_estacao","estacao")]
  #substitui NA em algumas linhas pelo nome da estacao
  dados_estacao$estacao[is.na(dados_estacao$estacao)] <- estacao
  save(dados_estacao, file = paste(diretorioRaiz(), "/","dados/",estacao,"_oeste_sc_", ano, ".RData",sep=""))
  
  estacao_oeste_sc <- merge(x = dados_estacao, y = depara_estacoes, by = "cod_estacao", all = TRUE)
  # substituindo NA para zero
  estacao_oeste_sc$precipitacao[is.na(estacao_oeste_sc$precipitacao)] <- 0
  # substituindo NA para nome da estacao
  estacao_oeste_sc$estacao[is.na(estacao_oeste_sc$estacao)] <- estacao
  # omite linhas com NA
  estacao_oeste_sc <- na.omit(estacao_oeste_sc)
  
  # Se tenho duas ou mais estacoes no mesmo municipio, faço uma média de precipitação das estações 
  # encontradas nos municípios. Preciso disso para fazer o merge
  estacao_oeste_sc_distintos <- estacao_oeste_sc[ ,list(precipitacao=mean(precipitacao)), by=c("CD_MUN")]
  
  # Para combinar os dados do objeto dados_sp com a base de dados de nosso 
  # shapefile, podemos utilizar a função merge():
  shp_dados_oeste_municipios_sc <- merge(x = shp_oeste_municipios_sc,
                                         y = estacao_oeste_sc_distintos,
                                         by.x = "codigo",
                                         by.y = "CD_MUN")
  
  #shp_dados_oeste_municipios_sc %>% 
  #  kable() %>%
  #  kable_styling(bootstrap_options = "striped", 
  #                full_width = TRUE, 
  #                font_size = 12)
  
  # Utilizando a tmap: ------------------------------------------------------
  bbox_new <- st_bbox(shp_dados_oeste_municipios_sc) # current bounding box
  
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  # bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
  # bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top
  
  bbox_new <- bbox_new %>%  # take the bounding box ...
    st_as_sfc() # ... and make it a sf polygon
  
  # bordas aos polígonos
  img <- tm_shape(shp = shp_dados_oeste_municipios_sc, bbox = bbox_new) + 
    tm_fill(col = "precipitacao", title="Precipitações",
            style = "quantile", 
            n = 4, 
            palette = "viridis", 
            legend.hist = FALSE,
            textNA="Sem Estações Meteorológicas") +
    tm_layout(legend.position = c("right", "top"),
              legend.text.size = 0.7,
              legend.title.size = 0.7,
              legend.hist.size = 0.7,
              legend.hist.height = 0.7,
              legend.hist.width = 0.7,
              frame = F,
              main.title = paste("Chuvas - Outono de ", ano, " - ", shp_dados_oeste_municipios_sc$abbrv_s,sep="")) +
    tm_borders(alpha = 0.5)
  
  #tmap_save(img, paste(diretorioRaiz(), "/", "dados/html/",estacao,"_",ano,".html", sep=""))
  
}