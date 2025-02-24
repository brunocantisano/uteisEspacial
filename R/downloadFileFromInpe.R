#' Multiplica por 2
#'
#' Esta função recebe o diretório onde será escrito o download, o nome e a extensão do arquivo a ser feito o download
#' @param diretorio Caminho onde será escrito o arquivo
#' @param arquivo arquivo a ser feito o download
#' @param extensao extensão do arquivo a ser feito o download
#' @return um arquivo raster carregado.
#' @export
downloadFileFromInpe <- function(diretorio, arquivo, extensao){
  # Verificar se o diretório existe; se não, criá-lo
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    print(paste("Diretório criado:", diretorio))
  }
  print(paste("Diretório já existe:", diretorio))
  pathZip = paste(diretorio, arquivo, extensao, sep="")
  pathTif = paste(diretorio, arquivo, ".tif", sep="")
  downloadUrl <- paste0('http://www.dsr.inpe.br/topodata/data/geotiff/',arquivo, extensao)
  
  if(!arquivo.exists(pathZip)){
    print(paste('Faço download:',arquivo))
    download.file(url=downloadUrl,destfile=pathZip, method='curl')
  }
  
  if(!arquivo.exists(pathTif)){
    print("descompactando arquivo zip")
    
    unzip(pathZip,exdir = paste(diretorio, sep=""))
  }
  
  print("carregando arquivo tif")
  
  # carregando o objeto raster
  return (raster(pathTif))
}