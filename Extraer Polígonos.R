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
library(sf)
library(ggplot2)
library(dplyr)

# Función para leer y plotear el shapefile
plotear_shapefile <- function(shapefile_ruta, folder_de_salida) {
  # Lee el shapefile
  datos <- st_read(shapefile_ruta)
  
  # Muestra el gráfico del shapefile completo
  mapa <- ggplot() +
    geom_sf(data = datos, fill = "lightblue", color = "black", alpha = 0.5) + # Ajusta alpha para transparencia
    theme_minimal() +
    ggtitle("Visualización del shapefile completo")
  
  # Muestra el gráfico en la ventana de plot
  print(mapa)
  
  # Muestra los nombres de los campos de la tabala de atributos
  cat("Nombres de los campos disponibles:\n")
  print(names(datos))
  
  # Solicita el nombre del campo a elegir por usuario (Escribir tal cual está el nombre del campo en la tabala de atributos)
  nombre_campo <- readline(prompt = "Ingresa el nombre del campo que deseas usar para extraer los polígonos: ")
  
  # Verifica si el campo existe
  if (!nombre_campo %in% names(datos)) {
    cat("El campo ingresado no existe.\n")
    return(NULL)
  }
  
  # Muestra los valores únicos del campo seleccionado por el usuario
  cat("Valores únicos en el campo", nombre_campo, ":\n")
  print(unique(datos[[nombre_campo]]))
  
  # Solicita los valores de los polígonos al usuario (Escribir tal cual se muestra el nombre o valor en ese campo. No espaciar despues de la coma)
  valores_poligonos <- readline(prompt = paste("Ingresa los valores de los polígonos que deseas extraer del campo", nombre_campo, "separados por comas: "))
  
  # Convierte los valores ingresados en una lista de valores
  valores_poligonos <- unlist(strsplit(valores_poligonos, ","))
  
  # Filtra los polígonos seleccionados
  poligonos_seleccionados <- datos %>% filter(.data[[nombre_campo]] %in% valores_poligonos)
  
  # Verifica si se encontraron polígonos
  if (nrow(poligonos_seleccionados) == 0) {
    cat("No se encontraron polígonos con los valores seleccionados en el campo.\n")
  } else {
    # Crea el gráfico de los polígonos seleccionados
    mapa <- ggplot() +
      geom_sf(data = poligonos_seleccionados, fill = "blue", color = "black", alpha = 0.2) + # Ajusta alpha para transparencia
      theme_minimal() +
      ggtitle(paste("Polígonos seleccionados:", paste(valores_poligonos, collapse = ", ")))
    
    # Muestra el gráfico en la ventana de plot
    print(mapa)
    
    # Guarda cada polígono seleccionado en un nuevo shapefile
    for (valor in valores_poligonos) {
      poligono_individual <- datos %>% filter(.data[[nombre_campo]] == valor)
      archivo_de_salida <- file.path(folder_de_salida, paste0(valor, ".shp"))
      st_write(poligono_individual, archivo_de_salida, delete_dsn = TRUE)
      
      cat("El polígono con valor", valor, "ha sido guardado en:", archivo_de_salida, "\n")
    }
  }
}

# Ruta del archivo shapefile (modificar según interés del usuario)
shapefile_ruta <- "C:/AdaptarC+/Plantilla de mapa - AdaptarC+/1. Shapes/Municipios_CHA.shp"

# Carpeta de salida donde se guardará el polígono extraído (modificar según interés del usuario)
folder_de_salida <- "C:/TESIS OSCAR"

# Llamado de la función
plotear_shapefile(shapefile_ruta, folder_de_salida)
