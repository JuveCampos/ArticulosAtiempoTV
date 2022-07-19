# Nuevo mapa electoral de México:

# Librerias ----
library(tidyverse)
library(sf)
library(ggtext)

# Datos: ----
cat_edos <- readRDS("01_Datos/cat_edos.rds")
elecciones <- readxl::read_xlsx("01_Datos/mapa electoral.xlsx", sheet = 3) %>%
  filter(Año == 2022)
mapa_electoral <- readxl::read_xlsx("01_Datos/mapa electoral.xlsx") %>%
  select(cve_ent, ent, `2021`, `2022`) %>%
  pivot_longer(cols = c(`2021`, `2022`))


unique(mapa_electoral$`2022`)
shp_edos <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>%
  select(cve_ent = CVE_EDO, ENTIDAD)
mapa <- right_join(shp_edos, mapa_electoral)
# openxlsx::write.xlsx(cat_edos, "01_Datos/edos_excel.xlsx")

# Visualización: ----
mapa_1 <- mapa %>%
  mutate(name = str_replace_all(name, c("2021" = "Mapa electoral <b>2021</b>",
                                        "2022" = "Mapa electoral <b>2022</b>"))) %>%
  mutate(hay.elecciones = ent %in% elecciones$Elección)

col_partidos <- tribble(
  ~Partido, ~Color,
  "PAN", "#0049d1",
  "PRI", "#de0f00",
  "PRD", "#ded300",
  "MC", "#de8d00",
  "PVEM", "#00de55",
  "MORENA", "#a30000",
  "NA", "#02ada2",
  "PANAL", "#02ada2",
  "PES" , "purple",
  "INDEPENDIENTE", "#b3009b",
  "PT", "#b33c00",
  "SIN VOTOS", "gray") %>%
  filter(Partido %in% mapa_1$value)

colores <- col_partidos$Color
names(colores) <- col_partidos$Partido

mapa_1 %>%
  ggplot(aes(fill = value)) +
  geom_sf(aes(alpha = hay.elecciones),
          color = "white") +
  scale_fill_manual(values = colores) +
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  labs(title = "¿Cómo quedó conformado el mapa de gobernadores de México?",
       subtitle = "Estados gobernados por las distintas fuerzas políticas.",
       fill = "Partidos Políticos") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  facet_wrap(~name)

# Guardamos:
ggsave("03_Visualizaciones/mapa_cambio.png",
       device = "png",
       height = 8, width = 10)

mapa_electoral <- readxl::read_xlsx("01_Datos/mapa electoral.xlsx")

# Población gobernada por cada partido:
pop <- readxl::read_xlsx("01_Datos/proyeccion_poblacion_conapo.xlsx") %>%
  filter(year %in% c(2021, 2022)) %>%
  left_join(cat_edos) %>%
  filter(cve_ent != "00") %>%
  group_by(year) %>%
  mutate(pp = 100*(valor/sum(valor))) %>%
  left_join(mapa_electoral %>% mutate(year = as.numeric(name)),
            by = c("cve_ent", "year")) %>%
  # View()
  group_by(value, year) %>%
  summarise(pp = sum(pp))

col_partidos <- tribble(
  ~Partido, ~Color,
  "PAN", "#0049d1",
  "PRI", "#de0f00",
  "PRD", "#ded300",
  "MC", "#de8d00",
  "PVEM", "#00de55",
  "MORENA", "#a30000",
  "NA", "#02ada2",
  "PANAL", "#02ada2",
  "PES" , "purple",
  "INDEPENDIENTE", "#b3009b",
  "PT", "#b33c00",
  "SIN VOTOS", "gray") %>%
  filter(Partido %in% pop$value)

colores <- col_partidos$Color
names(colores) <- col_partidos$Partido

pop %>%
  ggplot(aes(x = reorder(value, -pp), y = pp,
             color = value,
             fill = value)) +
  geom_col(color = "white") +
  geom_text(aes(label = str_c(round(pp, 2), "%")),
            hjust = 0.5,
            vjust = -1,
            size = 4,
            family = "Mulish",
            fontface = "bold") +
  scale_fill_manual(values = colores) +
  scale_color_manual(values = colores) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0),
                     label = scales::comma_format(suffix = "%")) +
  labs(subtitle = "Porcentaje de la población gobernada por partido. ",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cuanta gente gobernará MORENA desde las gubernaturas de los estados?",
       caption = "Fuente: Resultados electorales del INE con datos de la proyección de población de CONAPO para 2021 y 2022, por eso la variación en decimales de entidades que no participaron en elecciones. ") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", family = "Mulish"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish", size = 17),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  facet_wrap(~year)

ggsave("03_Visualizaciones/barras_poblacion.png",
       device = "png",
       height = 8, width = 13)

