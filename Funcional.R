#Mapa de gasolinerias y de TARs 
#Ultima edición: 15-05-2019
#Ultimo editor: IP

#Falta medir distancias entre TARS y Gasolineras, se recomienda usar


#Cargar paquetes, yo ya los tengo por lo tanto no requiere instalación, en caso contrario instalar.

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(readxl)


#Base de datos
gasolineras <- read_excel("gasolineras.xlsx")


# Construir el espacio del mapa
ui <- fluidPage(
  mainPanel( 
    leafletOutput(outputId = "mymap")
  )
)

# Construir función de lanzamiento.
server <- function(input, output, session) {
  
  
  #definir colores para estaciones y TArs
  pal <- colorNumeric(
    palette = c('red', 'black'),
    domain = gasolineras$TAR_Estacion) 
  
  #crear el mapa
  output$mymap <- renderLeaflet({
    leaflet(gasolineras) %>% 
      setView(lng = -99, lat = 16, zoom = 5)  %>% #México es la vista (1 es global, 15 es una cuadra)
      addTiles() %>% 
      addCircles(data = gasolineras, lat = ~ latitud, lng = ~ longitud, weight = 1, radius = ~sqrt(TAR_Estacion)*1000, color = ~pal(TAR_Estacion), fillOpacity = 0.2)
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = gasolineras)
    proxy %>% clearMarkers()
    
  })
}
#lanzar la aplicación
shinyApp(ui, server)

#medir la distancias
library(geosphere)

disTAR <- distm(c(gasolineras$longitud, gasolineras$latitud), c(lon = -99.84708, lat = 16.84064), fun = distHaversine)
