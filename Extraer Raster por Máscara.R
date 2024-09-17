# Junior Omar Hernández-Ortiz
# Laboratorio de Hidrología Isotópica
# Centro Experimental y de Innovaciópn del Recurso Hídrico - CEIRH
# Instituto Hondureño de Ciencias de la Tierra
# Facultad de Ciencias
# Universidad Nacional Autonoma de Honduras
# Correo Institucional: junior.hernandez@unah.edu.hn
# Correo Personal: omarhortiz33@gmail.com
# ****************************************************************************************************************************

# Librerías necesarias
library(sf) # Librería de datos espaciales
library(terra) # Libreria de datos espacailaes ráster
library(ggplot2) # Librería de graficos
library(dplyr) # Librería parte de tidyverse para la manipulación y transformación de datos

# Función para recortar el raster utilizando un polígono tipo shapefile como máscara
cortar_raster <- function(ruta_shapefile, ruta_raster, ruta_folder) {
  # Lee el shapefile
  shape_datos <- st_read(ruta_shapefile)
  
  # Muestra el gráfico del shapefile completo
  mapa <- ggplot() +
    geom_sf(data = shape_datos, fill = "lightblue", color = "black", alpha = 0.5) + 
    theme_minimal() +
    ggtitle("Visualización del Shapefile de Entrada Completo")
  
  # Muestra el gráfico en la ventana de plot
  print(mapa)
  
  # Muestra los nombres de los campos disponibles
  cat("Nombre de los campos de la tabla de atributos del shapefile:\n")
  print(names(shape_datos))
  
  # Solicita el nombre del campo al usuario
  nombre_campo <- readline(prompt = "Ingrese el nombre del campo del cual desea seleccionar el polígono o polígonos: ")
  
  # Verifica si el campo existe
  if (!nombre_campo %in% names(shape_datos)) {
    cat("El campo ingresado no existe.\n")
    return(NULL)
  }
  
  # Muestra los valores únicos del campo seleccionado
  cat("Valores únicos en el campo", nombre_campo, ":\n")
  print(unique(shape_datos[[nombre_campo]]))
  
  # Solicita los valores de los polígonos al usuario (separados por comas y no espaciados después de la coma)
  valores_poligonos <- readline(prompt = paste("Ingrese los valores de los polígonos que desea extraer del campo", nombre_campo, "separados por comas: "))
  
  # Divide los valores ingresados en una lista
  lista_poligonos <- unlist(strsplit(valores_poligonos, split = ","))
  
  # Lee el raster utilizando terra
  raster <- rast(ruta_raster)
  
  # Lista para almacenar los rasters cortados para visualización
  lista_rasters_cortados <- list()
  
  # Itera sobre cada valor de polígono y hace el corte individual
  for (valor_poligono in lista_poligonos) {
    # Filtra el polígono seleccionado
    poligono_seleccionado <- shape_datos %>% filter(.data[[nombre_campo]] == valor_poligono)
    
    # Verifica si el polígono existe
    if (nrow(poligono_seleccionado) == 0) {
      cat("No se encontró un polígono con el valor:", valor_poligono, "en el campo seleccionado.\n")
      next
    } else {
      # Muestra el gráfico del polígono seleccionado
      corte_seleccionado <- ggplot() +
        geom_sf(data = poligono_seleccionado, fill = "turquoise", color = "black", alpha = 0.7) + 
        theme_minimal() +
        ggtitle(paste("El polígono seleccionado es:", valor_poligono))
      
      # Muestra el gráfico en la ventana de plot
      print(corte_seleccionado)
      
      # Recorta el raster utilizando el polígono seleccionado sin preocuparse por el CRS
      raster_cortado <- mask(raster, vect(poligono_seleccionado))
      
      # Guarda el resultado en un nuevo archivo raster
      archivo_salida <- file.path(ruta_folder, paste0(valor_poligono, ".tif"))
      writeRaster(raster_cortado, archivo_salida, overwrite = TRUE)
      
      # Añade el raster cortado a la lista para visualización
      lista_rasters_cortados[[valor_poligono]] <- raster_cortado
      
      cat("El raster cortado para el polígono", valor_poligono, "ha sido guardado en:", archivo_salida, "\n")
    }
  }
  
  # Visualiza el o los rásters cortados
  if (length(lista_rasters_cortados) > 0) {
    cat("\nVisualizando los rásters cortados...\n")
    for (valor_poligono in names(lista_rasters_cortados)) {
      cat("Visualizando el raster cortado para el polígono:", valor_poligono, "\n")
      plot(lista_rasters_cortados[[valor_poligono]], main = paste("Raster cortado:", valor_poligono))
    }
  } else {
    cat("No se recortaron rasters.\n")
  }
}

# Ruta del archivo shapefile (modificar según su archivo interés)
ruta_shapefile <- "C:/AdaptarC+/Plantilla de mapa - AdaptarC+/1. Shapes/Municipios_CHA.shp"

# Ruta del archivo raster (modificar según su archivo de interés)
ruta_raster <- "C:/AdaptarC+/Plantilla de mapa - AdaptarC+/2. Raster/Raster_area_estudio.tif"

# Carpeta de salida donde se guardarán los rasters recortados (modificar según su carpeta de interés)
ruta_folder <- "C:/TESIS"

# Llamado a la función
cortar_raster(ruta_shapefile, ruta_raster, ruta_folder)