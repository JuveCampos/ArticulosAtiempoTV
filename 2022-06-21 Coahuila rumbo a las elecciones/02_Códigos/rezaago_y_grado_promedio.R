# graficas de indicadores ----
library(tidyverse)

# Datos ----
cat_muni <- readxl::read_xlsx("01_Datos/catalogo_municipal(1).xlsx")
rezago <- readxl::read_xlsx("01_Datos/Rezago_social_coneval.xlsx")
grado_educ <- readxl::read_xlsx("01_Datos/grado_promedio_escolaridad.xlsx")

grado_educ %>%
  filter(year == max(year)) %>%
  mutate(grado = case_when(valor < 6 ~ "Primaria Inconclusa",
                           between(valor, 6, 9) ~ "Primaria terminada y cursado algún grado de secundaria",
                           between(valor, 9, 12) ~ "Secundaria terminada y cursado algún grado de bachillerato"
                           )) %>%
  filter(str_extract(cve_mun, pattern = "^\\d\\d") == "05") %>%
  left_join(cat_muni) %>%
  ggplot(aes(x = reorder(municipio, valor),
             y = valor,
             fill = grado)) +
  geom_col() +
  labs(y = NULL, x = NULL,
       title = "¿Cuales son los municipios con el valor del\ngrado de educación promedio más alto?",
       subtitle = "Años de educación promedio por municipio",
       caption = "Fuente: Censo de Población y Vivienda 2020") +
  scale_y_continuous(expand = expansion(c(0,0.2), 0),
                     labels = scales::comma_format())+
  scale_fill_manual(values = wesanderson::wes_palettes$Zissou1 ) +
  geom_text(aes(label = str_c(format(round(valor, 2), nsmall = 2))),
            hjust = -0.1) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(family = "Poppins"),
        # axis.ticks = element_blank(),
        # panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        panel.border = element_blank(),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10, hjust = 1),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 12),
        plot.title.position = "plot")

ggsave("03_Visualizaciones/grafica_grado_educ.png",
       height = 8, width = 8, device = "png")

rezago %>%
  filter(year == max(year)) %>%
  filter(str_extract(cve_mun, pattern = "^\\d\\d") == "05") %>%
  left_join(cat_muni) %>%
  ggplot(aes(x = reorder(municipio, -valor),
             y = valor)) +
  geom_col() +
  coord_flip()


