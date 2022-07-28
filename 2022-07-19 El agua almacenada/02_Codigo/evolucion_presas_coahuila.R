# Librerias: ----
library(tidyverse)
library(sf)

# Datos ----
root <- "01_Datos/Almacenamiento_presas/Presas Coahuila/"
archivos <- str_c(root, list.files(root))

datos <- lapply(archivos, readxl::read_xlsx) %>%
  do.call(rbind, .)

bd_p <- datos %>%
  janitor::clean_names() %>%
  mutate(pp = 100*(almacenamiento_actual_hm3/namo_almacenamiento_hm3)) %>%
  mutate(fecha = as.Date(fecha)) %>%
  mutate(nombre_de_la_presa = str_replace_all(nombre_de_la_presa, c(
    "Presa la Amistad parte Mexicana, Coah." = "Internacional La Amistad, Coah.",
    "Presa la Amistad parte Mexicana" = "Internacional La Amistad, Coah.")))

bd_p_last <- bd_p %>%
  group_by(nombre_de_la_presa) %>%
  filter(fecha == max(fecha))

# names(bd_p)

pal <- c("#0A1045", "#00C2D1", "#2BC016", "#D00000", "#ED33B9")
bd_p %>%
  ggplot(aes(x = fecha,
             y = pp,
             group = nombre_de_la_presa,
             color = nombre_de_la_presa)) +
  geom_vline(xintercept = as.Date(str_c("01-01-", 2018:2022),
                                  format = "%d-%m-%Y"),
             color = "gray50",
             linetype = 2) +
  geom_line() +
  ggrepel::geom_label_repel(data = bd_p_last,
            aes(label = nombre_de_la_presa),
            hjust = 0,
            direction = "y") +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     labels = scales::comma_format(suffix = "%")) +
  scale_x_date(expand = expansion(c(0.05,0.2), 0)) +
  scale_color_manual(values = pal) +
  labs(y = "Porcentaje de almacenamiento con respecto\na su nivel máximo",
       x = NULL,
       title = "Porcentaje de almacenamiento de las principales presas de Coahuila",
       subtitle ="2018-2022",
       caption = "Fuente: SINA-CONAGUA. Volumen de las presas de México") +
  theme_bw() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish",
                                  size = 15,
                                  face = "bold",
                                  hjust = 0),
        plot.subtitle = element_text(family = "Mulish",
                                  size = 15,
                                  hjust = 0),
        plot.caption = element_text(family = "Mulish",
                                  size = 12,
                                  hjust = 0),
        axis.text = element_text(family = "Mulish",
                                    size = 12,
                                    hjust = 0.5),
        axis.title = element_text(family = "Mulish",
                                 size = 12,
                                 hjust = 0.5),
        plot.title.position = "plot")

ggsave("03_Gráficas/evolucion_presas_coahuila.png",
       device = "png",
       height = 6,
       width = 10)

# Mapa presas:

jenkifyer <- function(x, groups = 5, etiquetas = 1:5){

  library(BAMMtools)
  limits = BAMMtools::getJenksBreaks(x, groups + 1)

  ubicaciones = lapply(1:groups,
                       function(i){
                         # i = 2
                         which(between(x, limits[i], limits[i + 1]))
                         # which(x <= limits[i + 1] & x >= limits[i] )
                       })

  vector_nuevo = rep(NA, length(x))
  for(j in 1:groups){
    # j = 2
    vector_nuevo[ubicaciones[[j]]] <- etiquetas[j]
    # table(vector_nuevo)
  }
  # table(vector_nuevo)
  return(vector_nuevo)

}

# Mapa de las presas:
munis <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson") %>%
  filter(CVE_ENT == "05")

presas_mapa <- read_sf("01_Datos/Almacenamiento_presas/presas/presas.shp")
st_crs(presas_mapa)

unique(presas_mapa$ESTADO)

presas_coah <- presas_mapa %>%
  filter(ESTADO == "Coahuila de Zaragoza") %>%
  mutate(q = jenkifyer(VOL_NAMO, groups = 6, etiquetas = 1:6)) %>%
  arrange(-VOL_NAMO)



presas_coah %>%
  filter(NOM_OFICIA == "VENUSTIANO CARRANZA") %>%
  ggplot() +
  geom_sf(data = munis, fill = "#f5e3ba",
          color = "gray50",
          alpha = 0.2) +
  geom_sf(aes(size = VOL_NAMO),
          color = "navyblue") +
  scale_size_area() +
  theme_void() +
  theme(legend.position = "none")


