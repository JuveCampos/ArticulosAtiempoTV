library(tidyverse)
library(ggtext)

fallecimientos_ss <- readRDS("01_Datos/datos_ssalud_fallecimientos.rds")
fallecimientos_ss  %>%
  mutate(grupo_edad = case_when(between(EDAD, 0, 0) ~ "0 años",
                                between(EDAD, 1, 10) ~ "1 a 10 años",
                                between(EDAD, 11, 18) ~ "11 a 18 años",
                                between(EDAD, 19, 30) ~ "19 a 30 años",
                                between(EDAD, 31, 40) ~ "31 a 40 años",
                                between(EDAD, 41, 50) ~ "41 a 50 años",
                                between(EDAD, 51, 60) ~ "51 a 60 años",
                                between(EDAD, 61, 70) ~ "61 a 70 años",
                                between(EDAD, 71, 80) ~ "71 a 80 años",
                                EDAD >= 81 ~ "Mayores de 80 años")) %>%
  group_by(grupo_edad) %>%
  count() %>%
  ungroup() %>%
  mutate(pp = n/sum(n))



# Datos a nivel nacional ----

# length(unique(fall_coah$ID_REGISTRO))


# Datos a nivel estado ----
fall_coah = fallecimientos_ss %>%
  filter(ENTIDAD_RES == "05") %>%
  group_by(EDAD, SEXO) %>%
  count() %>%
  ungroup() %>%
  mutate(grupo_edad = case_when(between(EDAD, 0, 0) ~ "0 años",
                                between(EDAD, 1, 10) ~ "1 a 10 años",
                                between(EDAD, 11, 18) ~ "11 a 18 años",
                                between(EDAD, 19, 30) ~ "19 a 30 años",
                                between(EDAD, 31, 40) ~ "31 a 40 años",
                                between(EDAD, 41, 50) ~ "41 a 50 años",
                                between(EDAD, 51, 60) ~ "51 a 60 años",
                                between(EDAD, 61, 70) ~ "61 a 70 años",
                                between(EDAD, 71, 80) ~ "71 a 80 años",
                                EDAD >= 81 ~ "Mayores de 80 años")) %>%
  group_by(grupo_edad, SEXO) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  ungroup() %>%
  mutate(n = ifelse(SEXO == 1,
                    yes = n,
                    no = -n)) %>%
  mutate(hjust = ifelse(n <= 0,
                        1.2,
                        -0.2),
         y1 = ifelse(n <= 0,
                     -2300,
                     2300),
         y2 = ifelse(n <= 0,
                     n - 2300,
                     n + 2300)) %>%
  group_by(SEXO) %>%
  mutate(pp = 100*(n/sum(n)))

fall_coah %>%
  ggplot(aes(x = grupo_edad,
             y = n,
             fill = factor(SEXO))) +
  geom_col() +
  geom_text(aes(label = str_c(prettyNum(abs(n), big.mark = ","), "\n(", format(round(pp, 1), nsmall = 1), "%)"),
                hjust = hjust),
            family = "Mulish",
            # hjust = 0.5,
            size = 3) +
  scale_y_continuous(expand = expansion(c(0.4, 0.4), 0)) +
  coord_flip() +
  labs(title = "¿Cuánta gente ha fallecido por sexo y edad en Coahuila?",
       subtitle = "Pirámide de fallecimientos por sexo y edad al 4 de Julio del 2022",
       caption = "Fuente: Secretaría de Salud. Indivíduos que reportaron fecha de fallecimiento a lo largo de toda la base\ny que reportaron a la entidad 05 como su entidad de residencia. ",
       x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish",
                                  face = "bold", size = 15),
        axis.text.y = element_text(family = "Mulish",
                                  face = "bold", size = 10),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

ggsave("03_Visualizaciones/piramide_casos_covid_sexo.png",
       width  = 6, height = 6,
       device = "png")


# Datos gráfica:
fallecimientos_ss %>%
  filter(ENTIDAD_RES == "05") %>%
  group_by(SEXO) %>%
  count() %>%
  ungroup() %>%
  mutate(n/sum(n))
