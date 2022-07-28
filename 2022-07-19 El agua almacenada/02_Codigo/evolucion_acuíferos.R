library(tidyverse)
library(sf)

# Datos
root <- "01_Datos/Almacenamiento_acuiferos/"
carpetas <- list.files(root)

root_shps <- lapply(1:length(carpetas), function(i){
  str_c(str_c(root, carpetas[i]), "/",
        list.files(str_c(root, carpetas[i]), pattern = "shp$")  )
}) %>% unlist()

roots_tbl <- tibble(root = root_shps) %>%
  mutate(year = str_extract(root, pattern = "\\d+\\.shp") %>%
           str_remove(pattern = "\\.shp") %>%
           as.numeric())

acuiferos <- lapply(1:nrow(roots_tbl), function(x){
  # x = 1
  read_sf(roots_tbl$root[x]) %>%
    mutate(year = roots_tbl$year[x]) %>%
    select(id_acuif,nom_acuif, nom_edo, disp_hm3, dispon, year)
})

acuiferos_tbl <- acuiferos %>%
  do.call(rbind, .)

unique(acuiferos_tbl$nom_edo) %>% sort()

acuiferos_coah <- acuiferos_tbl %>%
  filter(nom_edo == "Coahuila de Zaragoza")

acuiferos_coah %>%
  ggplot(aes(fill = disp_hm3)) +
  geom_sf(color = "gray50") +
  scale_fill_gradientn(
    colors = c("brown","#E7B3AC", "white", "blue"),
    label = scales::comma_format(suffix = " Hm³")
    ) +
  labs(title = "Disponibilidad de agua en los acuíferos de Coahuila",
       subtitle = "Evolución 2011-2020 (año más reciente).",
       caption = "Fuente: Elaboración propia a partir de datos del SINA - CONAGUA. \nDatos de disponibilidad de los acuíferos\nhttp://sina.conagua.gob.mx/sina/tema.php?tema=acuiferos\n1 Hm³ equivale a mil millones de lítros",
       fill = "Disponibilidad (Hm³)") +
  facet_wrap(~year,
             ncol = 5) +
  theme_bw() +
  theme(
    legend.position = "bottom",
        plot.title = element_text(family = "Mulish",
                                  color = "brown",
                                  size = 18,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish",
                                     color = "brown",
                                     size = 15,
                                     hjust = 0),
        plot.caption = element_text(family = "Mulish",
                                    size = 8,
                                    hjust = 0),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(family = "Mulish",
                                  size = 15,
                                  hjust = 0.5,
                                  face = "bold"),
    legend.title = element_text(family = "Mulish",
                                size = 10,
                                hjust = 0.5,
                                face = "bold"),
    legend.text = element_text(family = "Mulish",
                               size = 8,
                               hjust = 0),
        plot.title.position = "plot") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barheight = 0.5,
                               barwidth = 20))

ggsave("03_Gráficas/evolucion_acuiferos_coahuila.png",
       device = "png",
       height = 6,
       width = 10)

# Acuíferos - Evolución de disponibilidad:
plt <- acuiferos_coah %>%
  as_tibble() %>%
  select(-geometry) %>%
  ggplot(aes(x = year, y = disp_hm3, group = nom_acuif, color = nom_acuif))+
  geom_line()

plotly::ggplotly(plt)

acuiferos_tbl %>%
  filter(year == 2020) %>%
  as_tibble() %>%
  arrange(disp_hm3) %>%
  View()

######## El 12avo acuífero con menor disponibilidad en 2020.

# Acuíferos con disponibilidad:
acuiferos_coah %>%
  as_tibble() %>%
  group_by(year) %>%
  count(dispon)

acuiferos_coah %>%
  filter(year %in% c(2011, 2020)) %>%
  ggplot(aes(fill = dispon)) +
  geom_sf(color = "#BCBBB5") +
  scale_fill_manual(
    values = c("#87Bc68", "#EB6B5D")) +
  labs(title = "Cambio en la categorización del acuífero de acuerdo a su disponibilidad de agua",
       subtitle = "Coahuila. Cambio de 2011 a 2020 (año más reciente con datos disponibles)",
       caption = "Fuente: Elaboración propia a partir de datos del SINA - CONAGUA. \nDatos de disponibilidad de los acuíferos\nhttp://sina.conagua.gob.mx/sina/tema.php?tema=acuiferos",
       fill = "Categoría") +
  facet_wrap(~year,
             ncol = 5) +
  theme_bw() +
  theme(
    # legend.position = "bottom",
    plot.title = element_text(family = "Mulish",
                              color = "brown",
                              size = 18,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(family = "Mulish",
                                 color = "brown",
                                 size = 15,
                                 hjust = 0.5),
    plot.caption = element_text(family = "Mulish",
                                size = 8,
                                hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(family = "Mulish",
                              size = 15,
                              hjust = 0.5,
                              face = "bold"),
    legend.title = element_text(family = "Mulish",
                              size = 15,
                              hjust = 0.5,
                              face = "bold"),
    legend.text = element_text(family = "Mulish",
                                size = 12,
                                hjust = 0),
    plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5,
                               barheight = 0.5,
                               barwidth = 20,
                             ncol = 1))

ggsave("03_Gráficas/cambio_categoria_acuiferos_coahuila.png",
       device = "png",
       height = 6,
       width = 10)



