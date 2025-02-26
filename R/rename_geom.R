#' Renomeia a coluna geométrica de um objeto sf para "geometry"
#'
#' Esta função verifica se um objeto `sf` possui uma coluna geométrica com um nome 
#' diferente de "geometry" e a renomeia para "geometry", garantindo compatibilidade 
#' com padrões esperados em análises espaciais.
#'
#' @param obj Um objeto do tipo `sf` (Simple Features).
#' @return O mesmo objeto `sf`, mas com a coluna geométrica renomeada para "geometry", se necessário.
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' names(nc)  # Pode ter um nome diferente para a coluna geométrica
#' nc <- rename_geom(nc)
#' names(nc)  # Agora a coluna geométrica será "geometry"
rename_geom <- function(obj) {
  geom_name <- attr(obj, "sf_column")
  if (!is.null(geom_name) && geom_name != "geometry") {
    names(obj)[names(obj) == geom_name] <- "geometry"
  }
  return(obj)
}