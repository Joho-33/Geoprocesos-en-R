# Junior Omar Hernández-Ortiz
# Laboratorio de Hidrología Isotópica
# Centro Experimental y de Innovaciópn del Recurso Hídrico - CEIRH
# Instituto Hondureño de Ciencias de la Tierra
# Facultad de Ciencias
# Universidad Nacional Autonoma de Honduras
# Correo Institucional: junior.hernandez@unah.edu.hn
# Correo Personal: omarhortiz33@gmail.com
# ****************************************************************************************************************************

# Librerías a utilizar
library(sf) 
library(ggplot2) 

# Ruta de las carpetas de los archivos shapefile de entrada y de de salida. Modificar según se requiera. Colocar al final de la carpeta de salida el nombre con el que se desea guaradar el buffer. (En este  caso es buffer.shp)
shapefile <- "C:/AdaptarC+/Plantilla de mapa - AdaptarC+/1. Shapes/Red Vial/Red Vial Choluteca Alta.shp"
carpeta_salida <- "C:/TESIS OSCAR/buffer_33m.shp"

# Definición de la función para cargar un shapefile y crear un buffer a una distancia dada
crear_buffer <- function(shapefile, carpeta_salida) {
  
  # Lee el archivo shapefile
  polilinea <- st_read(shapefile)
  
  # Visualiza el archivo shapefile
  mapa_1 <- ggplot(data = polilinea) +
    geom_sf(color = "green") +
    theme_minimal() +
    labs(title = "Archivo Shapefile Polilínea de Entrada",
         x = "Longitud",
         y = "Latitud") +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  print(mapa_1)
 
  # Solicita la distancia del buffer a crear. Sólo colocar un número real positivo. Las unidades son metros puesto que el shape de entrada está en coordenadas UTM  
  distancia_buffer <- as.numeric(readline(prompt = "Ingrese la distancia para el buffer: "))
  
  # Crea el buffer alrededor de la polilínea con la distancia ingresada
  buffer_polilinea <- st_buffer(polilinea, dist = distancia_buffer)
  
  # Visualiza el resultado del buffer
  mapa_2 <- ggplot() +
    geom_sf(data = buffer_polilinea, fill = "lightblue", alpha = 0.5) +
    geom_sf(data = polilinea, color = "orange") +
    theme_minimal() +
    labs(title = "Archivo Buffer de Salida",
         x = "Longitud",
         y = "Latitud") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(mapa_2)
  
  # Guarda el buffer en un archivo shapefile en la ruta de carpeta de salida
  st_write(buffer_polilinea, carpeta_salida)
  
  # Confirmación del geoproceso
  print(paste("El archivo buffer se ha guardado en:", carpeta_salida))
}

# Llamado de la función para generar el buffer
crear_buffer(shapefile, carpeta_salida)