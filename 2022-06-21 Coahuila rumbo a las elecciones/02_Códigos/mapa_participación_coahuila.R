
library(tidyverse)
library(sf)

shape <- readRDS("../2022-06-14 Resultados 2022/01_Datos/shape.rds") %>%
  filter(entidad == "05")

d_part_2017 <- readxl::read_xlsx("01_Datos/Gobernador_Xcasilla.xlsx") %>%
  select(municipio, seccion, TOTAL, LISTA_NOMINAL) %>%
  group_by(municipio, seccion) %>%
  summarise(TOTAL = sum(TOTAL, na.rm = T),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  ungroup() %>%
  mutate(participacion = 100*(TOTAL/LISTA_NOMINAL)) %>%
  mutate(CVE_MUN = str_c("05", ifelse(str_length(municipio) == 1,
                                      yes = str_c("00", municipio),
                                      no = str_c("0", municipio))))

mapa <- left_join(shape, d_part_2017, by = c("seccion")) %>%
  mutate(participacion = ifelse(participacion > 100, yes = 100, no = participacion))

mapa %>%
  ggplot(aes(fill = participacion)) +
  geom_sf(color = "transparent")


