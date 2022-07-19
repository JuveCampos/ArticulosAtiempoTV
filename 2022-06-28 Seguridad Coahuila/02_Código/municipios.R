library(tidyverse)
library(sf)
library(cartogram)

# Datos: ----
hom_muni <- readxl::read_xlsx("01_Datos/INEGI homicidios con Municipios Coahuila.xlsx")

hom_muni_2 <- hom_muni %>%
  mutate(year = as.numeric(year)) %>%
  pivot_longer(cols = 2:ncol(.),
               values_to = "Homicidios INEGI",
               names_to = "Municipios") %>%
  filter(year >= 2000) %>%
  mutate(`Homicidios INEGI` = as.numeric(`Homicidios INEGI`))


hom_muni_2 %>%
  filter(year == 2012) %>%
  arrange(-`Homicidios INEGI`)

municipios <- c("Torreón",
  "Saltillo",
  "Piedras Negras",
  "Ramos Arizpe",
  "Matamoros",
  "San Pedro",
  "Monclova",
  "Acuña")

hom_muni_2 %>%
  filter(Municipios %in% municipios) %>%
  ggplot(aes(x = year, y = `Homicidios INEGI`)) +
  geom_col(fill = "#780101") +
  geom_text(aes(label = prettyNum(`Homicidios INEGI`, big.mark = ",")),
            vjust = -0.5,
            family = "Impact",
            color = "black",
            size = 4) +
  facet_wrap(~Municipios, ncol = 2
             , scales = "free_y"
             ) +
  labs(x = NULL, y = NULL,
       title = "¿Cómo han evolucionado los datos de Homicidios en los municipios de Coahuila?",
       subtitle = "Evolución 2000-2020. ",
       caption = "Datos de homicidios de INEGI") +
  scale_y_continuous(expand = expansion(c(0, 0.3), 0)) +
  scale_x_continuous(breaks = 2000:2022) +
  theme_bw() +
  theme(strip.text = element_text(family = "Impact", size = 15, color = "#780101"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, family = "Impact", vjust = 0.5, size = 14),
        plot.title = element_text(family = "Impact", size = 26),
        plot.subtitle = element_text(family = "Impact", size = 20),
        plot.caption = element_text(family = "Impact", size = 10),
        text = element_text(family = "Impact"),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave("03_Visualizaciones/evolucion_homicidios_municipios.png",
       width = 13,
       height = 14,
       device = "png")

# Tasa por cada 10.000 habitantes:
cat_muni <- readxl::read_xlsx("01_Datos/catalogo_municipal(1).xlsx")
proy_muni <- readxl::read_xlsx("01_Datos/proyeccion_municipal_conapo.xlsx")
pob_muni <- left_join(proy_muni, cat_muni)
pob_muni_coah <- pob_muni %>%
  filter(cve_ent == "05") %>%
  left_join(hom_muni_2, by = c("municipio" = "Municipios",
                               "year")) %>%
  mutate(tasa_10000 = `Homicidios INEGI`/(valor/10000)) %>%
  filter(!is.na(tasa_10000))

pob_muni_coah %>%
  filter(year == 2021)


# Mapa Homicidios Concentración:

cat_coah <- cat_muni %>% filter(str_extract(cve_mun, pattern = "\\d\\d") == "05") %>%
  mutate(municipio = str_replace_all(municipio, c("Cuatrociénegas" = "Cuatro Ciénegas")))

shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>%
  filter(CVE_ENT == "05")
mapa <- hom_muni_2 %>%
  filter(year > 2010) %>%
  group_by(Municipios) %>%
  summarise(suma_homicidios = sum(`Homicidios INEGI`, na.rm = T)) %>%
  left_join(cat_muni %>% filter(str_extract(cve_mun, pattern = "\\d\\d") == "05"),
            by = c("Municipios" = "municipio"))

mapa$cve_mun[is.na(mapa$cve_mun)] <- "05007"
mapa$cve_ent[is.na(mapa$cve_ent)] <- "05"

mapa %>%
  left_join(proy_muni %>% filter(year == 2021)) %>%
  mutate(por_1000 = suma_homicidios/ponderador) %>%
  arrange(-por_1000)


mapa_x <- left_join(shp, mapa, by = c("CVEGEO" = "cve_mun")) %>%
  st_transform(crs = 2163)


# Dorling:
dorling = cartogram_dorling(st_transform(na.omit(mapa_x[mapa_x$suma_homicidios > 0,]),
                                         2163),
                            "suma_homicidios")

dorling = dorling %>%
  mutate(area = st_area(.),
         X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])
# %>%
#   mutate(tamanio = case_when(pp > 20 ~ "grande",
#                              between(pp, 5, 20) ~ "mediano",
#                              between(pp, 1, 5) ~ "pequeño",
#                              pp <= 1 ~ "muy pequeño")) %>%
#   mutate(tamanio = factor(tamanio, levels = c("grande", "mediano", "pequeño", "muy pequeño")))

dorling %>%
  ggplot() +
  geom_sf(data = mapa_x,
          color = "black",
          fill = "#f5e5bf",
          size = 0.1) +
  geom_sf(fill = "red") +
  ggrepel::geom_label_repel(aes(x = X, y = Y,label = str_c(NOM_MUN, "\n", prettyNum(suma_homicidios, big.mark = ","))),
                            family = "Impact", size = 3,
                            hjust = 0.5) +
  labs(x = NULL, y = NULL,
       title = "Homicidios por municipio",
       subtitle = "Suma de los registros de homicidios de INEGI, del 2010 al 2021",
       caption = "Datos de homicidios de INEGI") +
  theme_bw() +
  theme(strip.text = element_text(family = "Impact", size = 15, color = "#780101"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        plot.title = element_text(family = "Impact", size = 26),
        plot.subtitle = element_text(family = "Impact", size = 20),
        plot.caption = element_text(family = "Impact", size = 10),
        text = element_text(family = "Impact"),
        plot.title.position = "plot",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave("03_Visualizaciones/mapa_homicidios_municipio.png",
       height = 10,
       width = 8)


