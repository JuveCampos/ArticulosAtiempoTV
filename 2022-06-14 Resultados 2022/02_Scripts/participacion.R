# Barras electoral

# Librerias ----
library(tidyverse)
library(cowplot)

# Datos ----

elecciones <- readxl::read_xlsx("01_Datos/mapa electoral.xlsx", sheet = 2)

e1 <- elecciones %>%
  filter(Coalicion %in% c("Participación", "LISTA NOMINAL")) %>%
  select(-c(Fuente, Coalicion, Porcentaje)) %>%
  pivot_wider(id_cols = Entidad,
              values_from = Absoluto,
              names_from = Representante) %>%
  mutate(abstencionismo =  100*((`LISTA NOMINAL` - Participación)/`LISTA NOMINAL`),
         abstencion =  `LISTA NOMINAL` - Participación)

abst <- tibble(Entidad = e1$Entidad,
       Coalicion = "Abstencionismo",
       Representante = "Abstencionismo",
       Absoluto = e1$abstencion,
       Porcentaje = e1$abstencionismo,
       Fuente = "",
       Participación = e1$Participación,
       `LISTA NOMINAL` = e1$`LISTA NOMINAL`)

e2 <- left_join(elecciones, e1 %>% select(1:3)) %>%
  rbind.data.frame(abst) %>%
  mutate(pp = 100*(Absoluto/`LISTA NOMINAL`)) %>%
  filter(!(Representante %in% c("Participación", "LISTA NOMINAL")))

naran <- left_join(elecciones, e1 %>% select(1:3)) %>%
  rbind.data.frame(abst) %>%
  mutate(pp = 100*(Absoluto/`LISTA NOMINAL`)) %>%
  filter(Representante %in% c("MC"))

naran %>%
  ggplot(aes(x = reorder(Entidad, -Porcentaje),
             y = Porcentaje)) +
  geom_col(fill = "orange") +
  geom_hline(yintercept = 3,
             color = "red",
             linetype = 2,
             size = 2) +
  geom_text(aes(label = str_c(round(Porcentaje, 2), "%")),
            vjust = -1,
            fontface = "bold") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  labs(x = NULL, y = NULL,
       subtitle = "Porcentaje de votación con respecto a los votos emitidos",
       title = "¿Cuáles fueron los resultados de Movimiento Ciudadano?") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold",
                                   family = "Mulish", angle = 90,
                                   hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text = ggtext::element_markdown(family = "Mulish", size = 17),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot")

ggsave("03_Visualizaciones/naranja.png",
       device = "png",
       width = 10,
       height = 5)


part <-
  left_join(elecciones, e1 %>% select(1:3)) %>%
  rbind.data.frame(abst) %>%
  mutate(pp = 100*(Absoluto/`LISTA NOMINAL`)) %>%
  filter(Representante %in% c("Participación"))

part %>%
  ggplot(aes(x = reorder(Entidad, -Porcentaje),
             y = Porcentaje)) +
  geom_col() +
  geom_text(aes(label = str_c(round(pp, 2), "%")),
            vjust = -1,
            fontface = "bold") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  labs(x = NULL, y = NULL,
       subtitle = "Porcentaje de participación",
       title = "¿Cuáles fueron las entidades con mayor participación?") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold",
                                   family = "Mulish", angle = 90,
                                   hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text = ggtext::element_markdown(family = "Mulish", size = 17),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot")

ggsave("03_Visualizaciones/participacion.png",
       device = "png",
       width = 10,
       height = 5)

col_partidos <- tribble(
  ~Partido, ~Color,
  "I_1", "pink",
  "I_2", "purple",
  "MAS", "navyblue",
  "PAN", "#0049d1",
  "PRI", "#de0f00",
  "PRD", "#ded300",
  "MC", "#de8d00",
  "PVEM", "#00de55",
  "MORENA", "#a30000",
  "FXM", "pink",
  "NA", "#02ada2",
  "Abstencionismo", "gray10")

colores <- col_partidos$Color
names(colores) <- col_partidos$Partido

e <- unique(e2$Entidad)

plots <- lapply(seq_along(e), function(x){

  # x = 4
  plt <- e2 %>%
    filter(Entidad == e[x])

  p <- plt %>%
    ggplot(aes(x = reorder(str_wrap(Coalicion), -pp),
               y = pp,
               fill = Representante)) +
    geom_col() +
    geom_text(aes(label = str_c(round(pp, 2), "%"),
                  color = Representante),
              vjust = -1,
              fontface = "bold"
              ) +
    scale_fill_manual(values = colores)  +
    scale_color_manual(values = colores)  +
    scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
    labs(x = NULL, y = NULL,
         subtitle = plt$Entidad[1]
         # title = "Porcentaje de votos con respecto a la lista nominal"
         ) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold",
                                     family = "Mulish", angle = 90,
                                     hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          strip.background = element_rect(fill = "white"),
          strip.text = ggtext::element_markdown(family = "Mulish", size = 17),
          plot.title = element_text(family = "Mulish", hjust = 0.5,
                                    face = "bold", size = 18),
          plot.caption = element_text(size = 10),
          plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
          plot.title.position = "plot") +
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5))

  return(p)
})

# rm(plots)

plot_grid(plotlist = plots)

ggsave("grid_abstencion.png",
       device = "png",
       height = 10,
       width = 10)

