# Junior Omar Hernández-Ortiz
# Laboratorio de Hidrología Isotópica
# Centro Experimental y de Innovaciópn del Recurso Hídrico
# Instituto Hondureño de Ciencias de la Tierra
# Facultad de Ciencias - UNAH
# Correo Institucional: junior.hernandez@unah.edu.hn
# Correo Personal: omarhortiz33@gmail.com
# ****************************************************************************************************************************

# Librerías a utilizar
library(sf) 
library(ggplot2) 

# Definición de la función para cargar un shapefile y crear un buffer a una distancia dada por el usuario
crear_buffer <- function() {
  
  # Solicita la ruta del archivo .shp de entrada en la consola
  shapefile <- readline(prompt = "Ingrese la ruta del archivo shapefile de entrada: ")
  
  # Valida que la ruta no esté vacía
  if (shapefile == "") {
    stop("Hay un error. La ruta del archivo shapefile está vacía.")
  }
  
  # Lee el archivo .shp, con validación de errores
  polilinea <- tryCatch({
    st_read(shapefile)
  }, error = function(e) {
    stop("Hay un error. Verifique que la ruta sea correcta.")
  })
  
  # Visualiza el archivo shapefile
  mapa_1 <- ggplot(data = polilinea) +
    geom_sf(color = "green") +
    theme_minimal() +
    labs(title = "Archivo Shapefile Polilínea de Entrada",
         x = "Longitud",
         y = "Latitud") +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  print(mapa_1)
  
  # Solicita la distancia del buffer a crear. Sólo colocar un número real positivo. Las unidades son metros.
  distancia_buffer <- as.numeric(readline(prompt = "Ingrese la distancia (metros) para el buffer: "))
  
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
  
  # Solicita la ruta de salida para el archivo shapefile del buffer
  carpeta_salida <- readline(prompt = "Ingrese la ruta de salida del archivo buffer, incluyendo el nombre del archivo con terminación —extensión— .shp: ")
  
  # Valida que la ruta de salida no esté vacía
  if (carpeta_salida == "") {
    stop("Se ha presentado un error. La ruta de salida no puede estar vacía.")
  }
  
  # Guarda el buffer en un archivo shapefile en la ruta de carpeta de salida
  st_write(buffer_polilinea, carpeta_salida)
  
  # Confirma que el geoproceso se ha realizado bien
  print(paste("El archivo buffer se ha guardado en:", carpeta_salida))
}

# Llamado de la función para generar el buffer al compilar el código
crear_buffer()
