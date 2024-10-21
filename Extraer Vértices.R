# Junior Omar Hernández-Ortiz
# Laboratorio de Hidrología Isotópica
# Centro Experimental y de Innovaciópn del Recurso Hídrico
# Instituto Hondureño de Ciencias de la Tierra
# Facultad de Ciencias - UNAH
# Correo Institucional: junior.hernandez@unah.edu.hn
# ****************************************************************************************************************************

library(sf)
library(terra)
library(ggplot2)

# Lee el archivo .shp del polígono
poligono <- st_read("C:/Proyectos QGIS/Santa Cruz, Lempira/Shapes/Municipio de Santa Cruz.shp")

# Detecta el sistema de referencia de coordenadas (CRS) del shapefile de entrada
crs_original <- st_crs(poligono)

# Extrae los vértices del polígono con geometría de punto
vertices <- st_cast(poligono, "POINT")

# Agrega los campos con las coordenadas X,Y en el sistema de referencia original (UTM)
vertices$X <- st_coordinates(vertices)[, 1]
vertices$Y <- st_coordinates(vertices)[, 2]

# Transforma las coordenadas geográficas (Longitud, Latitud) si el CRS original no es 4326
if (crs_original$epsg != 4326) {
  vertices_georreferencia <- st_transform(vertices, crs = 4326) # 4326 es el código EPSG para WGS84 (coordenadas geográficas)
} else {
  vertices_georreferencia <- vertices
}

# Agrega los campos de las coordenadas Longitud y Latitud (Geográficas WGS84)
vertices$Long <- st_coordinates(vertices_geo)[, 1]
vertices$Lat <- st_coordinates(vertices_geo)[, 2]

# Guarda los vértices en un nuevo shapefile con geometría de puntos
archivo_de_salida <- "C:/Proyectos QGIS/vertices_Santa_Cruz.shp"
st_write(vertices, archivo_de_salida, delete_layer = TRUE)

# Lee el shapefile de vertices guardado
archivo_de_vertices <- st_read(archivo_de_salida)

# Grafica el shapefile de salida para visualización de los vértices extraídos
mapa <- ggplot() +
  geom_sf(data = archivo_de_vertices, aes(geometry = geometry), color = "orange", size = 1) +
  theme_minimal() +
  labs(title = "Vértices del Polígono") +
  theme(plot.title = element_text(hjust = 0.5))

print(mapa)
