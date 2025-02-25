#' Multiplica por 2
#'
#' Esta função recebe o diretório onde será escrito o download, o nome do arquivo a ser feito o download
#' @param diretorio Caminho onde será escrito o arquivo
#' @param arquivo arquivo a ser feito o download
#' @return um arquivo raster carregado.
#' @export
downloadFileFromInpe <- function(diretorio, arquivo){
  arquivoSemExtensao <- tools::file_path_sans_ext(arquivo)
  diretorio <- paste(diretorio, "dados/relevo/", sep="")
  # Verificar se o diretório existe; se não, criá-lo
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    print(paste("Diretório criado:", diretorio))
  }
  pathZip = paste(diretorio, arquivo, sep="")
  pathTif = paste(diretorio, arquivoSemExtensao, ".tif", sep="")
  print(pathZip)
  print(pathTif)
  downloadUrl <- paste0('http://www.dsr.inpe.br/topodata/data/geotiff/',arquivo)
  
  if(!file.exists(pathZip)){
    print(paste('Faço download:',arquivo))
    download.file(url=downloadUrl,destfile=pathZip, method='curl')
  }
  
  if(!file.exists(pathTif)){
    print("descompactando arquivo zip")
    
    unzip(pathZip,exdir = paste(diretorio, sep=""))
  }
  
  print("carregando arquivo tif")
  
  # carregando o objeto raster
  return (raster(pathTif))
}