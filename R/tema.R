#' Funcion generica para temas
#'
#' Temas para graficas (ggplot, ploty o highcharter)
#'
#' @param tipo tipo de grafica (hc, ggplot, o plotly)
#' @param colores esquema de colores, generalmente el default (tse1)
#' @export
tse_theme <- function(tipo = "hc") {
  if(tipo == "hc"){


  }

  if(tipo == "ggplot"){

  }

  if(tipo == "plotly"){

  }
}

#' Exporta paletas de colores
#' 
#' Escoges una paleta y exporta colores correspondientes.
#' 
#' @param paleta paleta de colores, en numero (1). 
#' @details Paleta 1: creada por KFG (25-01-2017)
#' @export
tse_colores <- function(paleta){
  
  p <- switch(paleta,
         # paleta en colores de rgba
         c(rgb(3,7,27,166, maxColorValue = 255), #Azul 1
           rgb(11,22,80,166, maxColorValue = 255), #Azul 2
           rgb(16,33,121,166, maxColorValue = 255), #Azul 3
           rgb(25,52,188,166, maxColorValue = 255), #Azul 4
           rgb(67,94,228,166, maxColorValue = 255), #Azul 5
           rgb(135,152,237,166, maxColorValue = 255), #Azul 6
           rgb(188,197,245,166, maxColorValue = 255), #Azul 7
           rgb(228,232,251,166, maxColorValue = 255)), #Azul 8 
         # paleta en colores hex
         c("#44546a", #azul 1 ppt
           "#adb9ca", #azul 2 ppt
           "#d6dce5", #azul 3 ppt
           "#002060", #azul fuerte 
           "#a6a6a6", #gris
           "#7f7f7f", # gris fuerte
           "#f8cbb2", #rosa
           "#ea7a7a" #rosa fuerte
          ))
  p <- as.character(p)
  p
}
