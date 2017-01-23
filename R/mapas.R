#' Importar KML con Codigos o AGEBS
#' 
#' Importa en R un KML exportado por entidad oficial, para usarse en crear mapas.
#' @param archivo nombre de archivo
t_impkml <- function(archivo){
  require(rgdal)
  require(rworldmap)
  require(plotKML)
  require(maptools)
  
  f <- readOGR(archivo)
  print(summary(f))
  f
}

#' Exportar KML con rangos de colores
#' 
#' Exporta un archivo KML con atributos de colores, para usar en Google Maps. Usa la funcion de Tom Philippi 
#' (disponible en: https://dl.dropboxusercontent.com/content_link/DcDBdbghu5D7eM6tBQ5cVGMNGYXaSzaj3CZfcYSc9vjUDW3yd3KrTynMotNZoi4n/file)
#' Exporta en loop el objeto SpatialPolygonsDataFrame a un KML. Todos los parametros se pasan como strings. 
#' @param obj objeto SpatialPolygonsDataFrame, pasado como string.
#' @param archivo nombre de archivo al que se va exportar.
#' @param col_nombre columna en data.frame con nombre de cada objeto.
#' @param col_desc columna con descripcion en data.frame para cada objeto.
#' @param col_color columna con id's de color en mapa de colores.
#' @param mapa_colores data.frame con columna de id y color para mapear los id's a un color #RRGGBBAA form (AA=alpha 00=transparent FF=solid). Defaults a: "mapa_colores_topaz".
#' @param mapa_nombre nombre de mapa (capa) que se visualiza en Google Earth. Defaults a: "Mapa_TSE".
#' @param mapa_desc descripcion del mapa (capa) que se visualiza en Google Earth. Defaults a: "Mapa creado por PE de TSE".
t_expkml <- function(obj, archivo, 
                     col_nombre, col_desc, col_color, 
                     mapa_colores = "mapa_colores_topaz", 
                     mapa_nombre = "Mapa_TSE", mapa_desc = "Mapa Creado por PE de TSE") {
  
  # - Esto me revisa la proyeccion y cambia a WGS84 cuando es otra
  if (proj4string(get(obj))!="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
    cat("Input SpatialPolygonsDataFrame ",obj," re-projected from ",
        proj4string(get(obj))," to WGS84 longlat\n",sep="")
    assign(obj,spTransform(get(obj),CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")))
  } # check projection
  
  # Abre un archivo temporal
  kmlFile <- file(archivo, 
                  open = "w")
  
  # Abre slot de polygonos, llena en out
  # usa maptools::kmlPolygon
  Xout <- sapply(slot(get(obj), 
                      "polygons"), 
                 function(x) { 
                   maptools::kmlPolygon(x, # Objeto es de una funcion anonima 
                                        name = as(get(obj), "data.frame")[slot(x, "ID"), col_nombre], 
                                        # el nombre lo pega como un data.frame del slot ID (subset por id)
                                        col = get(mapa_colores)[as.numeric(as(get(obj), "data.frame")[slot(x, "ID"), col_color])],
                                        # pega el color, pero deacuerdo al ID y tema de color (subsets por id)
                                        lwd = 0.5, # tamano de stroke
                                        border = NA, # borders
                                        visibility = TRUE, #visible poligono
                                        description = as(get(obj),"data.frame")[slot(x, "ID"), col_desc], # descripcion, igual en df
                   ) 
                 }
  )
  
  # exporta polyono out
  cat(kmlPolygon(kmlname=mapa_nombre, 
                 kmldescription=mapa_desc)$header, 
      file = kmlFile, sep="\n")
  # estilos
  cat(unlist(Xout["style",]), 
      file = kmlFile, sep="\n")
  # contenidos
  cat(unlist(Xout["content",]), 
      file = kmlFile, sep="\n")
  # footers
  cat(kmlPolygon()$footer, 
      file = kmlFile, sep="\n")
  close(kmlFile) # cierra temporal / archivo que escribio
}
#' Reduce un KML a otra geografia
#' 
#' Hace un subset de una geografia, para revisar una zona particular de un KML. 
#' @param obj Objeto SpatialPolygonsDataFrame
#' @param condicion 
#' 
kml_cortar <- function(obj, condicion) {
  sub <- eval(substitute(condicion), 
              envir=obj, enclos=parent.frame())
  cat("Recuerda quitar factores que han cambiado (para exportar solamente lo necesario). \n I.e: foo$Name <- factor(foo$Name)")
  obj[sub, ]
}




