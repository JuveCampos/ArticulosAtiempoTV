# Librerias:
library(tidyverse)
library(sf)

# Datos:
edos = read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

plot(edos[1,])

# Fondo blanco:
lapply(1:nrow(edos), function(i){

  edo = edos[i,]$ENTIDAD[1]
  edos[i,] %>%
    ggplot() +
    geom_sf(fill = "white", color = "white") +
    theme_void()

  ggsave(str_c("../../00_Assets/estados_siluetas/blanco_transparente/",
               edo,
               ".png"),
         device = "png",
         height = 1,
         width = 1)

})


# Bolita
library(geosphere)


i = 2

lapply(1:nrow(edos), function(i){
edo = edos[i,]$ENTIDAD[1]
shp = edos[i,]
ctrd <- shp %>% st_centroid()
c <- st_coordinates(shp) %>%
  as_tibble() %>%
  summarise(min_x = min(X),
            max_x = max(X),
            min_y = min(Y),
            max_y = max(Y))
distancia = distm(c(c$min_x, c$min_y), c(c$max_x, c$max_y), fun = distHaversine)

buffer <- ctrd %>%
  st_buffer(dist = distancia/1.7)

shp %>%
  ggplot() +
  geom_sf(data = buffer,
          color = "white",
          size = 2,
          fill = "#056947") +
  geom_sf(fill = "white", color = "white") +
  theme_void()

ggsave(str_c("../../00_Assets/estados_siluetas/bolita_verde//",
             edo,
             ".png"),
       device = "png",
       height = 2,
       width = 2)

})
