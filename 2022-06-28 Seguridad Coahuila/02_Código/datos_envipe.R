# Victimización:
library(tidyverse)



# tablas <- tibble::tribble(
#              ~V1,                                                                                                                                                                                   ~V2,
#              5.1,                                  "Población de 18 años y más por entidad federativa y temas que generan mayor preocupación según la percepción de la población marzo y abril de 2021",
#              5.4,                                                 "Población de 18 años y más por entidad federativa, según percepción sobre la seguridad en colonia o localidad marzo y abril de 2021",
#              5.7,                                 "Población de 18 años y más por entidad federativa, según percepción sobre la seguridad en municipio o demarcación territorial marzo y abril de 2021",
#              5.1,                                                  "Población de 18 años y más por entidad federativa, según percepción sobre la seguridad en entidad federativa marzo y abril de 2021",
#             5.13,                                         "Población de 18 años y más por entidad federativa y espacio público o privado, según percepción de seguridad en éstos marzo y abril de 2021",
#             5.16, "Población de 18 años y más por entidad federativa, según percepción de seguridad que siente al caminar solo(a) por la noche en los alrededores de su vivienda marzo y abril de 2021",
#             5.19,                "Población de 18 años y más por entidad federativa y actividad cotidiana, según condición de haberla dejado de realizar por temor a ser víctima de algún delito, 2020",
#             5.22,    "Población de 18 años y más por entidad federativa e incidentes delictivos, según percepción acerca de la posibilidad de ser víctima de alguno de ellos marzo a diciembre de 2021",
#             5.28,                  "Población de 18 años y más por entidad federativa, según percepción sobre la tendencia de la seguridad pública en su colonia o localidad marzo a diciembre de 2021",
#             5.31,                   "Población de 18 años y más por entidad federativa, según percepción sobre la tendencia de la seguridad pública en su entidad federativa marzo a diciembre de 2021",
#             5.34,                                  "Población de 18 años y más por entidad federativa, según percepción sobre la tendencia de la seguridad pública en México marzo a diciembre de 2021"
#             )

#
# readxl::read_xlsx("01_Datos/conjunto_de_datos_ENVIPE_2021_csv/V_percepcion_seguridad_2021_est.xlsx",
#                   sheet = tablas$V1[1],
#                   skip = 6) %>%
#   janitor::clean_names() %>%
  # filter(!is.na(entidad_federativa))

envipe <- readxl::read_xlsx("01_Datos/conjunto_de_datos_ENVIPE_2021_csv/Datos_percepción.xlsx") %>%
  filter(Concepto != "Percepción de seguridad al caminar solo por la noche ")

unique(envipe$Concepto)
envipe %>%
  group_by(Concepto) %>%
  summarise(anios = unique(year))

datos_por_año <- envipe %>%
  filter(Entidad != "Estados Unidos Mexicanos") %>%
  group_by(Concepto, year) %>%
  mutate(ranking = rank(-Porcentaje)) %>%
  arrange(Concepto, year, ranking)

datos_por_año %>%
  filter(year == 2021) %>%
  filter(Entidad == "Coahuila de Zaragoza")



deltas <- datos_por_año %>%
  pivot_wider(id_cols = 1:2,
              values_from = c("Porcentaje"),
              names_from = "year") %>%
  mutate(delta = `2021` - `2019`) %>%
  ungroup() %>%
  group_by(Concepto) %>%
  mutate(ranking_delta = rank(-delta)) %>%
  arrange(Concepto, -delta) %>%
  relocate(`2019`, .before = `2021`)

deltas %>%
  mutate(`Ranking en 2019` = rank(-`2019`),
         `Ranking en 2021` = rank(-`2021`)) %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  openxlsx::write.xlsx("03_Visualizaciones/tabla_resultados_percepcion_Coahuila.xlsx")


deltas %>%
  filter(!is.na(delta)) %>%
  ggplot(aes(x = reorder(Entidad, `2021`), y = `2021`)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Concepto)







