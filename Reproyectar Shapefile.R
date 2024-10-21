# Junior Omar Hernández-Ortiz
# Laboratorio de Hidrología Isotópica
# Centro Experimental y de Innovaciópn del Recurso Hídrico
# Instituto Hondureño de Ciencias de la Tierra
# Facultad de Ciencias - UNAH
# Correo Institucional: junior.hernandez@unah.edu.hn
# ****************************************************************************************************************************

library(sf)
library(ggplot2)

# Lee la ruta de entrada del archivo shapefile
archivo_de_entrada <- "C:/Proyectos QGIS/Santa Cruz, Lempira/Shapes/Municipio de Santa Cruz.shp"

# Función para reproyectar el shapefile
reproyectar_shapefile <- function(archivo_de_entrada, archivo_de_salida) {
  # Lee el shapefile
  shapefile <- st_read(archivo_de_entrada)
  
  # Lee el CRS original
  crs_original <- st_crs(shapefile)
  
  # Muestra el CRS original
  if (grepl("UTM", crs_original$proj4string, ignore.case = TRUE)) {
    cat("El CRS original es un sistema de coordenadas UTM:\n", crs_original$proj4string, "\n")
  } else if (crs_original$epsg == 4326) {
    cat("El CRS original es WGS84 (EPSG: 4326)\n")
  } else {
    cat("El CRS original es:", crs_original$proj4string, "\n")
  }
  
  # Solicita al usuario que ingrese —en la consola— el EPSG al cual desea reproyectar 
  cat("Por favor, ingrese el EPSG al cual desea reproyectar:\n")
  crs_destino <- as.integer(readline())
  
  # Reproyecta el shapefile
  shapefile_reproyectado <- st_transform(shapefile, crs_destino)
  
  # CRS reproyectado
  crs_reproyectado <- st_crs(shapefile_reproyectado)
  
  # Muestra el CRS reproyectado
  cat("El CRS reproyectado es:", crs_reproyectado$proj4string, "\n")
  
  # Guarda el shapefile reproyectado
  st_write(shapefile_reproyectado, archivo_de_salida, delete_layer = TRUE)
  
  cat("El shapefile ha sido reproyectado y guardado en:", archivo_de_salida, "\n")
  
  # Grafica el archivo reproyectado
  mapa_reproyectado <- ggplot() +
    geom_sf(data = shapefile_reproyectado, aes(geometry = geometry), color = "orange", size = 0.5, alpha = 0.5) +
    theme_minimal() +
    labs(title = "Archivo Reproyectado") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(mapa_reproyectado)
}

# Ruta de salida con el nombre del archivo a guardar con la terminación .shp
archivo_de_salida <- "C:/Proyectos QGIS/Reproyectado3.shp"

# Llamado de la función para reproyectar
reproyectar_shapefile(archivo_de_entrada, archivo_de_salida)
