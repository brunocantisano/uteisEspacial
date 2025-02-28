#' Retorna dataframe
#'
#' Esta função recebe um  shapefile e busca as coordenadas do oeste de sc
#' @param shp shapefile
#' @return dataframe com nomes trocados
#' @export
busca_coords_oeste_sc <- function(shp) {
  l <- shp@polygons
  codigos <- shp@data$codigo
  municipios <- shp@data$regiao
  size <- length(l)
  i <-1
  df <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("codigo", "municipio", "latitude", "longitude"))))
  for (elem in l) {
    coords <- elem@Polygons[[1]]@labpt
    codigo <- codigos[i]
    municipio <- municipios[i]
    longitude <- coords[1]
    latitude <- coords[2]
    #coords_all <- elem@Polygons[[1]]@coords
    
    df <- rbind(df, list(codigo, municipio, latitude, longitude))
    
    i <- i + 1
  }
  names(df)[1] <- 'codigo'
  names(df)[2] <- 'municipio'
  names(df)[3] <- 'latitude'
  names(df)[4] <- 'longitude'
  return (df)
}