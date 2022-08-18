# Librerias
library(tidyverse)

# Datos:
bd <- readxl::read_xlsx("01_Datos/base_minas.xlsx")
bd$Estado

bd %>%
  filter(Estado == "Coahuila") %>%
  group_by(Año) %>%
  summarise(Fallecidos = sum(Fallecidos)) %>%
  mutate(is.2022 = Año == 2022) %>%
  ggplot(aes(x = Año, y = Fallecidos,
             fill = is.2022)) +
  geom_col() +
  scale_fill_manual(values = c("brown", "yellow")) +
  scale_x_continuous(expand = expansion(c(0.1, 0.1), 0.0),
                     breaks = seq(1880, 2020, 10)) +
  scale_y_continuous(expand = expansion(c(0, 0.1), 0.0),
                     breaks = seq(0, 400, 50)) +
  labs(title = "Fallecimientos en accidentes y siniestros mineros en Coahuila",
       subtitle = "Eventos históricos registrados en artículos de prensa",
       x = NULL, y = "Fallecimientos") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        text = element_text(family = "Mulish"),
        plot.title = element_text(size = 15,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.position = "none")

ggsave("02_Visualizaciones/barras_fallecimientos.png",
       height = 4, width = 7)



bd %>%
  filter(Estado == "Coahuila") %>%
  mutate(is.rosita = str_detect(str_to_lower(Lugar), pattern = "rosita")) %>%
  # group_by(Año) %>%
  # summarise(Fallecidos = sum(Fallecidos)) %>%
  # mutate(is.2022 = Año == 2022) %>%
  ggplot(aes(x = Año, y = Fallecidos,
             fill = is.rosita,
             alpha = is.rosita)) +
  geom_col() +
  scale_alpha_manual(values = c(0.6, 1)) +
  scale_fill_manual(values = c("brown", "orange")) +
  scale_x_continuous(expand = expansion(c(0.1, 0.1), 0.0),
                     breaks = seq(1880, 2020, 10)) +
  scale_y_continuous(expand = expansion(c(0, 0.1), 0.0),
                     breaks = seq(0, 400, 50)) +
  labs(title = "Fallecimientos en accidentes y siniestros mineros en Coahuila",
       subtitle = "Fallecimientos ocurridos en la mina <b style = 'color:orange;'>Rosita</b> en color naranja",

       x = NULL, y = "Fallecimientos") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        text = element_text(family = "Mulish"),
        plot.title = element_text(size = 15,
                                  face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 14),

        legend.position = "none")

ggsave("02_Visualizaciones/barras_fallecimientos_rosita.png",
       height = 4, width = 7)

# Puntos Coahuila

bd %>%
  filter(Año > 1960) %>%
  arrange(-Fallecidos) %>%
  head(10) %>%
  ggplot(aes(x = seq(1:10),
             y = 2,
             size = Fallecidos^3)) +
  geom_point() +
  geom_text(aes(label = str_c())) +
  scale_y_continuous(limits = c(1.99, 2.01))


bd %>%
  # filter(Año > 1960) %>%
  head(15) %>%
  select(Año, Lugar, Fallecidos, Causa)

bd %>%
  filter(str_detect(str_to_lower(Lugar), pattern = "rosita")) %>%
  pull(Año)

