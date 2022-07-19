# Librerias: ----
library(tidyverse)
library(sf)
library(cartogram)
library(rvest)
library(ggtext)

# Dias faltantes para la elección ----
as.Date("2023-06-04") - as.Date("2022-06-21")

# Geometrias de los municipios
geom <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson") %>%
  filter(NOM_ENT == "Coahuila de Zaragoza" )


# Población por comunidades ----
p2 <- readxl::read_xlsx("01_Datos/cpv2020_b_coa_01_poblacion.xlsx",
                        sheet = 2,
                        skip = 6) %>%
  filter(`...2` == "Total") %>%
  pivot_longer(5:ncol(.)) %>%
  filter(`...3` == "Población") %>%
  filter(value != 0) %>%
  mutate(pp = 100*(value/sum(value)))


# Población de la entidad: ----
names(p)
p <- readxl::read_xlsx("01_Datos/cpv2020_b_coa_01_poblacion.xlsx",
                       sheet = 3,
                       skip = 5) %>%
  janitor::clean_names() %>%
  filter(!is.na(entidad_federativa)) %>%
  filter(grupos_quinquenales_de_edad == "Total" & sexo == "Total")  %>%
  filter(municipio != "Total") %>%
  mutate(pp = 100*(poblacion_total1/sum(poblacion_total1))) %>%
  select(entidad_federativa, municipio, poblacion_total1, pp) %>%
  mutate(CVEGEO = str_c(str_extract(entidad_federativa, pattern = "^\\d+"),
                        str_extract(municipio, pattern = "^\\d+")))

# Grafica exploracion:
p %>%
  ggplot(aes(x = reorder(municipio, poblacion_total1),
             y = poblacion_total1)) +
  geom_col(fill = "#86a4d1") +
  geom_text(aes(label = str_c(round(pp, 2), "%")),
            hjust = -0.1, family = "Poppins", size = 3, fontface = "bold") +
  scale_y_continuous(expand = expansion(c(0,0.2), 0),
                     labels = scales::comma_format())+
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Poppins"),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot")

ggsave("03_Visualizaciones/barras_población.png",
       height = 8, width = 8, device = "png")

# Gráfica Dorling de población: ----
mapa <- left_join(geom, p)

dorling = cartogram_dorling(st_transform(na.omit(mapa[mapa$pp > 0,]),
                                         2163),
                            "pp")

dorling = dorling %>%
  mutate(area = st_area(.),
         X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2]) %>%
  mutate(tamanio = case_when(pp > 20 ~ "grande",
                             between(pp, 5, 20) ~ "mediano",
                             between(pp, 1, 5) ~ "pequeño",
                             pp <= 1 ~ "muy pequeño")) %>%
  mutate(tamanio = factor(tamanio, levels = c("grande", "mediano", "pequeño", "muy pequeño")))

plot(dorling, max.plot = 1)

dorling %>%
  ggplot(aes(fill = tamanio)) +
  geom_sf() +
  geom_text(aes(x = X,
                y = Y,
                label = str_c(str_trunc(str_wrap(NOM_MUN, 3), 20), "\n",
                              round(pp, 2), "%"),
                size = tamanio),
            color = "white") +
  scale_discrete_manual(aesthetics = "size",
                        values = c(3.8,2.5,1.5,0)) +
  scale_fill_manual(values = wesanderson::wes_palettes$BottleRocket1[-c(3)]) +
  labs(x = NULL, y = NULL,
       title = "Cartograma de Dorling de la\nPoblación por Municipios de Coahuila",
       caption = "Fuente: Elaboración con datos de INEGI, Censo de Población y Vivienda, 2020") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot")

ggsave("03_Visualizaciones/cartograma_población.png",
       height = 8, width = 8, device = "png")

# Encuestas ----

url <- "https://es.wikipedia.org/wiki/Elecciones_estatales_de_Coahuila_de_2023"
html <- read_html(url)

# html %>%
#   html_table() %>%
#   pluck(3) %>%
#   openxlsx::write.xlsx("01_Datos/encuestas.xlsx")

encuestas <- readxl::read_excel("01_Datos/encuestas.xlsx")  %>%
  pivot_longer(4:ncol(.)) %>%
  mutate(value = as.numeric(str_remove(value, pattern = "[\\s+]?\\%"))) %>%
  mutate(name = factor(name, levels = c("PAN",
                                           "PRI",
                                           "MC",
                                           "MORENA",
                                           "Otro",
                                           "Ninguno/No sabe")))

encuestas %>%
  ggplot(aes(x = Fecha_2,
             y = value,
             color = name)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_y_continuous(labels = scales::comma_format(suffix = "%")) +
  scale_x_datetime(breaks = "1 month") +
  scale_color_manual(values = c("blue",
                               "red",
                               "orange",
                               "brown",
                               "purple",
                               "gray")) +
  labs(y = NULL, x = NULL, color = "Partido",
       title = "¿Cómo van los partidos rumbo a las elecciones del 2023?",
       subtitle = "Resultados de encuestas de opinión de partidos\nEstado de Coahuila",
       caption = "Fuente: https://es.wikipedia.org/wiki/Elecciones_estatales_de_Coahuila_de_2023") +
  theme_bw() +
  theme(legend.position = "none",
        # axis.text = element_blank(),
        # axis.ticks = element_blank(),
        # panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 12),
        plot.title.position = "plot") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold")) +
  guides(color  = guide_legend(title.position = "top",
                               title.hjust = 0.5))

ggsave("03_Visualizaciones/evolución_encuestas.png",
       height = 6, width = 10, device = "png")

# Grafica de participación electoral Coahuila:
part <- readxl::read_xlsx("01_Datos/datos_varios.xlsx",
                          sheet = 2)

part %>%
  ggplot(aes(x = factor(Año),
             y = Participación,
             fill = Elección)) +
  geom_col() +
  geom_text(aes(label = str_c(round(Participación, 2),
                               "%")),
             vjust = -0.5,
            fontface = "bold",
            family = "Poppins") +
  scale_y_continuous(expand = expansion(c(0,0.2), 0),
                     labels = scales::comma_format(suffix = "%")) +
  theme_bw() +
  labs(x = NULL, title = "Participación electoral en Coahuila 2002-2022") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill  = guide_legend(title.position = "top",
                               title.hjust = 0.5))

# Gráfica sexos coahuila ----
sx <- readxl::read_xlsx("01_Datos/datos_varios.xlsx")
munis <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

sx %>%
  filter(sexo != "general") %>%
  ggplot(aes(x = 1, y = habitantes, fill = sexo)) +
  geom_col() +
  xlim(-1,1.5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot")


# Participación electoral por municipio ----
ppmuni <- readxl::read_xlsx("01_Datos/datos_varios.xlsx",
                            sheet = 3) %>%
  rename(participacion =`...5` ) %>%
  mutate(CVEGEO =ifelse(str_length(str_squish(Clave)) == 1,
                         yes = str_c("0500", Clave),
                         no = str_c("050", Clave)) %>%
           str_remove(pattern = "\\s"))

# ppmuni
munis %>% filter(CVE_ENT == "05") %>%
  ggplot() +
  geom_sf(fill = "#507d8a",
          color = "white") +
  theme_void()


left_join(munis %>% filter(CVE_ENT == "05"), ppmuni) %>%
  ggplot(aes(fill = participacion)) +
  geom_sf(color = "white") +
  scale_fill_gradientn(colors = wesanderson::wes_palettes$Zissou1[-2],
                       limits = c(38, 65),
                       labels = scales::comma_format(suffix = "%")) +
  labs(title = "¿Cómo fue la participación electoral\nen las últimas elecciones a gobernador?",
       subtitle = "Porcentaje de participación a nivel municipal, 2017",
       fill = "(%) Participación electoral") +
  guides(color  = guide_colorbar(title.position = "top",
                               title.hjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 12),
        plot.title.position = "plot") +
  guides(fill  = guide_colorbar(title.position = "top",
                              title.hjust = 0.5,
                              barwidth = 15,
                              barheight = 0.5))

ggsave("03_Visualizaciones/mapa_participacion.png",
       height = 8, width = 8, device = "png")


ppmuni %>%
  ggplot(aes(x = reorder(Nombre, participacion),
             y = participacion,
             fill = participacion)) +
  geom_col() +
  geom_text(aes(label = format(str_c(round(participacion, 2), "%"), nsmall = 2)
                ),
            hjust = -0.1,
            family = "Poppins",
            size = 3
            ) +
  coord_flip() +
  scale_fill_gradientn(colors = wesanderson::wes_palettes$Zissou1[-2],
                       limits = c(38, 65),
                       labels = scales::comma_format(suffix = "%")) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0),
                     labels = scales::comma_format(suffix = "%")) +
  labs(title = "¿Cómo fue la participación electoral en las últimas elecciones a gobernador?",
       subtitle = "Porcentaje de participación a nivel municipal, 2017",
       x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        # axis.text = element_blank(),
        # axis.ticks = element_blank(),
        # panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 12),
        plot.title.position = "plot") +
  guides(fill  = guide_legend(title.position = "top",
                                title.hjust = 0.5))

ggsave("03_Visualizaciones/barras_participacion.png",
       height = 8, width = 8, device = "png")

