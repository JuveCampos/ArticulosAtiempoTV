# Librerias:
library(tidyverse)

# Datos:
unique(inegi_agua$ent)
inegi_agua <- readxl::read_xlsx("01_Datos/censo_agua/Libro1.xlsx") %>%
  janitor::clean_names() %>%
  mutate(across(.cols = 3:(ncol(.)-1),
                .fns = function(x){
                  str_remove_all(x, pattern = ",") %>%
                    as.numeric()})
         ) %>%
  mutate(pp = 100*(disponen_de_cisterna/total)) %>%
  mutate(is.coa_nac = case_when(ent == "Coahuila de Zaragoza"~"Coahuila",
                                ent == "Nuevo León"~"Nuevo León",
                                ent == "Total" ~ "Nacional",
                                TRUE ~ "Otros")) %>%
  mutate(ent = str_replace(ent, pattern = "Total", replacement = "Nacional"))

unique(inegi_agua$concepto)

inegi_agua %>%
  filter(concepto == "Disponibilidad de cisterna o Aljibe") %>%
  ggplot(aes(x = reorder(ent, pp),
             y = pp,
             fill = is.coa_nac,
             alpha = is.coa_nac)) +
  geom_col() +
  geom_text(aes(label = str_c(prettyNum(round(pp, 2), big.mark = ","),
                              "%")),
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
  labs(title = "¿Cuál es el porcentaje de viviendas\nque almacenan su agua en cisternas o aljibes?",
       subtitle = "Porcentaje de viviendas que cuentan con disponibilidad de cisterna o aljibe",
       x = NULL, y = NULL, caption = "Fuente: Elaboración propia con datos del Censo de Población y Vivienda 2020\nTabulado de viviendas") +
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


inegi_agua %>%
  filter(concepto == "Disponibilidad de tinaco") %>%
  ggplot(aes(x = reorder(ent, pp),
             y = pp,
             fill = is.coa_nac,
             alpha = is.coa_nac)) +
  geom_col() +
  geom_text(aes(label = str_c(prettyNum(round(pp, 2), big.mark = ","),
                              "%")),
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
  labs(title = "¿Cuál es el porcentaje de viviendas\nque almacenan su agua en tinacos?",
       subtitle = "Porcentaje de viviendas que cuentan con disponibilidad de tinacos",
       x = NULL, y = NULL, caption = "Fuente: Elaboración propia con datos del Censo de Población y Vivienda 2020\nTabulado de viviendas") +
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






