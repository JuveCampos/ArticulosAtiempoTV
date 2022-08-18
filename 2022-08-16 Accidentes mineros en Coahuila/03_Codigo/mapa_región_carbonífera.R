
# Librerias:
library(tidyverse)
library(sf)
library(ggrepel)

# 01. Mapa región carbonífera ----

# Datos:
shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

coah <- shp %>%
  filter(CVE_ENT == "05") %>%
  mutate(sel = NOM_MUN %in% c("Sabinas",
                              "San Juan de Sabinas",
                              "Múzquiz",
                              "Juárez",
                              "Progreso"))

coah_et <- coah %>%
  filter(sel) %>%
  mutate(label = ifelse(sel == T,
                        yes = NOM_MUN,
                        no = NA)) %>%
  mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
         ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])


coah %>%
  ggplot(aes(fill = factor(sel))) +
  geom_sf(alpha = 0.5,
          color = "gray60") +
  geom_sf(data = coah_et,
          fill = "#446455",
          color = "white",
          linetype = 1,
          size = 0.5) +
  geom_label_repel(data = coah_et,
             aes(label = str_wrap(label, 10),
                 x = ctrd_x,
                 y = ctrd_y),
             color = "white",
             size = 4,
             family = "Poppins") +
  scale_fill_manual(values = c("#C7B19C",
                                "#446455")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("02_Visualizaciones/mapa_carbonifera.png",
       height = 10,
       width = 10)


# wesanderson::wes_palettes$Chevalier1 %>% scales::show_col()

