# Librerias:
library(tidyverse)

# Paleta de colores
c("#015794", "#1c689e", "#1c919e", "#1c9e77") %>%
  scales::show_col()

# Lluvia promedio acumulada por entidad:
lluvia <- readxl::read_xlsx("01_Datos/lluvia_promedio_entidad.xlsx")
cat_edos <- readRDS("01_Datos/cat_edos.rds")
lluvia <- left_join(lluvia, cat_edos) %>%
  filter(year == max(year)) %>%
  mutate(is.coa_nac = case_when(ent == "Coahuila"~"Coahuila",
                                ent == "Nuevo León"~"Nuevo León",
                                ent == "Nacional" ~ "Nacional",
                                TRUE ~ "Otros"))

lluvia %>%
  ggplot(aes(x = reorder(ent, valor),
             y = valor,
             fill = is.coa_nac,
             alpha = is.coa_nac)) +
  geom_col() +
  geom_text(aes(label = str_c(prettyNum(round(valor, 2), big.mark = ","),
                              "mm")),
            hjust = -0.1,
            size = 4,
            family = "Mulish",
            color = "gray10") +
  coord_flip() +
  scale_fill_manual(values = c("red",
                               "#1c689e",
                               "orange",
                               "#1c689e")
                    , guide = "none"
                    ) +
  scale_y_continuous(expand = expansion(c(0,0.4), 0)) +
  scale_alpha_manual(values = c(1, 1, 1, 0.7), guide = "none") +
  labs(title = "¿Cúanto llovió en 2021, en promedio,\nen cada entidad del país?",
    subtitle = "Precipitación promedio estatal 2021",
    x = NULL, y = "mm de lluvia", caption = "Fuente: SMN - CONAGUA, 2021 (último año completo).") +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        axis.text.y = element_text(angle = 0, family = "Mulish", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# lluvia en coahuila a lo largo del tiempo:
lluvia <- readxl::read_xlsx("01_Datos/lluvia_promedio_entidad.xlsx")
cat_edos <- readRDS("01_Datos/cat_edos.rds")
lluvia <- left_join(lluvia, cat_edos) %>%
  # filter(year == max(year)) %>%
  mutate(is.coa_nac = case_when(ent == "Coahuila"~"Coahuila",
                                ent == "Nuevo León"~"Nuevo León",
                                ent == "Nacional" ~ "Nacional",
                                TRUE ~ "Otros")) %>%
  filter(is.coa_nac != "Otros")

lluvia %>%
  filter(ent == "Coahuila") %>%
  pull(valor) %>%
  mean()

lluvia_fin <- lluvia %>%
  filter(year == max(year))




lluvia %>%
  ggplot(aes(x = year,
             y = valor,
             group = ent,
             color = is.coa_nac)) +
  geom_line() +
  geom_text(data = lluvia_fin,
            aes(x = 2021.5, label = ent),
            hjust = 0,
            family = "Mulish",
            fontface = "bold") +
  scale_y_continuous(label = scales::comma_format(suffix = " mm")) +
  scale_x_continuous(expand = expansion(c(0,0.3), 0),
                     breaks = 2003:2021) +
  scale_color_manual(values = c("red",
                               "#1c689e",
                               "orange",
                               "#1c689e"), guide = "none") +
  labs(title = "Evolución de la lluvia promedio por entidad",
       subtitle = "2003-2021. Estados seleccionados",
       x = NULL, y = "mm de lluvia", caption = "Fuente: SMN - CONAGUA.") +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        axis.text.y = element_text(angle = 0, family = "Mulish", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        # axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank())

