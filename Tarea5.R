#### Tarea 4 ####
####### Sara e  IVan###########

#### Paquetería ####

library(foreign)
library(geosphere)
library(ggplot2)
#remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
library(ggvoronoi)
library(h3jsr)
library(hereR)
library(igraph)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(magrittr)
library(pacman)
library(RCurl)
library(readr)
library(readxl)
library(rgdal)
#install.packages("rgeos", repos="http://R-Forge.R-project.org")
library(rgeos)
library(rvest)
library(sf)
library(sp)
library(tidyverse)

#### Cargamos el SpatialLine con los datos de las calles shape e del Marco Geoestadistico censo ####
shape=readOGR("09e.shp", layer="09e")

#Observamos la proyeccion INEGI
shape@proj4string

#Este archivo viene en el Marco Geoestadístico Nacional
shape@lines #Nota que son lines y no polygons

# Observamos que esto es lo que se solicita.
###### Moctezuma 2a Sección	Colonia	15530	Venustiano Carranza	Ciudad de México
###### Roma Norte	Colonia	06700	Cuauhtémoc	Ciudad de México	

####### Filtramos inicialmente a nivel municipio o alcaldía, que es lo que sí tenemos en la base
shp_data <- st_read("09e.shp")

# 017 es Venustiano Carranza y 015 es Cuhuatemoc
shp_filtrado <- shp_data %>%
  filter(CVE_MUN == "017"| CVE_MUN == "015")

#Guardamos el shp filtrado
shp_filtrado<-st_write(shp_filtrado, "shp_filtrado.shp",append=FALSE)

#Cargamos shape e del Marco Geoestadistico censo pero ya filtrado
shapef<-readOGR("shp_filtrado.shp", layer="shp_filtrado")


####### Filtrar el shp de código postel ####
## Considera las vialidades que se encuentran (aunque sea parcialmente) en los siguientes CP de la CDMX : 06700 y 15530.

shapecp<-readOGR("cp_09cdmx_v8_1.shp", layer="cp_09cdmx_v8_1")

#Observamos la proyeccion INEGI
shapecp@proj4string

# Filtramos inicialmente a CP
shp_data1 <- st_read("cp_09cdmx_v8_1.shp")

shp_filtradocp <- shp_data1 %>%
  filter(cp == "06700"| cp == "15530")

#Guardamos el shp filtrado
st_write(shp_filtradocp, "shp_filtradocp.shp",append=FALSE)

#Cargamos shape e del Marco Geoestadistico censo
shapecp<-readOGR("shp_filtradocp.shp", layer="shp_filtradocp")

#Graficamos
ggplot() +
  geom_sf(data = shp_filtradocp)

######### Cambiar el sistema de coordenadas #############
library(sp)
library(rgdal)
library(sf)

proj4string(shapecp)

# Transformar el sistema de coordenadas
mi_base_espacial <- st_read("shp_filtradocp.shp") # Ejemplo de carga desde un archivo shapefile
mi_base_transformada <- st_transform(mi_base_espacial, crs = "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
st_write(mi_base_transformada, "shp_filtradocpes.shp",append=FALSE)

shapecpt<-readOGR("shp_filtradocpes.shp", layer="shp_filtradocpes")
shapef<-readOGR("shp_filtrado.shp", layer="shp_filtrado")

############ Intersectamos ####

# Realizar la intersección
lines_inside_polygon <- gIntersection(shapef, shapecpt, byid = TRUE)

# Verificar el resultado
plot(lines_inside_polygon, col = "lightblue", main = "Líneas Dentro de las Colonias")

####### Creamos base de intersecciones
# Primero, convierte el SpatialLines a un objeto sf
lines_sf <- st_as_sf(lines_inside_polygon)

# Luego, guarda el objeto sf como un archivo shapefile
st_write(lines_sf, "lines_inside_polygon.shp",append=FALSE)
shapecpt<-readOGR("lines_inside_polygon.shp", layer="lines_inside_polygon")

lines_inside_polygon <- readOGR(dsn="lines_inside_polygon.shp")

# Inicializar un vector para almacenar las coordenadas de las intersecciones
intersections_list <- vector("list", length = 0)

# Calcular las intersecciones

for (i in 1:1050) {
  for (j in (i + 1):length(lines_inside_polygon)) {
    if (gIntersects(lines_inside_polygon[i, ], lines_inside_polygon[j, ])) {
      intersection <- gIntersection(lines_inside_polygon[i, ], lines_inside_polygon[j, ])
      if (!is.null(intersection)) {
        # Añadir las coordenadas de la intersección al vector
        coords <- coordinates(intersection)
        intersections_list[[length(intersections_list) + 1]] <- coords
      }
    }
  }
}

# Combinar todas las coordenadas en una sola matriz
if (length(intersections_list) > 0) {
  all_intersections <- do.call(rbind, intersections_list)
} else {
  all_intersections <- matrix(numeric(0), ncol = 2) # Matriz vacía si no hay intersecciones
}

all_intersections <- as.data.frame(all_intersections)

ggplot(all_intersections, aes(x = x, y = y)) + 
  geom_point() +
  theme_minimal()