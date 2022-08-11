# Librerias:
library(tidyverse)

# Defunciones registradas (mortalidad general)
# Consulta de: Defunciones generales  Por: Ent y mun de residencia y Lista mex enfermedades   Según: Año de registro

# Paleta:
vec_colores <- c("Otras enfermedades no transmisibles" = "#5CC9FA",
"Enfermedades del corazón" = "#EFA9A1",
"Diabetes mellitus" = "#FBE7A3",
"Tumor maligno" = "#F3DDF0",
"Accidentes" = "#D2A687",
"Agresiones (homicidios)" = "#Ea3323",
"Enfermedades transmisibles" = "#CADFB8",
"COVID-19" = "#4f6c35")


# Datos:
bd <- readxl::read_xlsx("01_Datos/INEGI_exporta_9_8_2022_16_59_38.xlsx") %>%
  mutate(CVE_ENT = ifelse(is.na(CVE_ENT), yes = "00", no = CVE_ENT))

bd2 <- bd %>%
  pivot_longer(cols = 4:ncol(.),
               names_to = "year",
               values_to = "valor") %>%
  mutate(year = as.numeric(year),
         valor = as.numeric(valor %>% str_remove_all(pattern = ",")),
         clave = str_extract(Enfermedad,
                             pattern = "\\(.*\\)") %>%
           str_remove_all(pattern = "\\(|\\)"),
         nivel = ifelse(str_detect(clave, pattern = "[A-Z]"),
                        yes = 2, no = 1))

unique(bd2$Entidad)

# Total de un estado ----
total <- bd2 %>%
  filter(nivel == 2) %>%
  filter(year == 2021) %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  arrange(-valor) %>%
  head(20)

# Enfermedades:
total$Enfermedad

total %>%
  ggplot(aes(x = reorder(Enfermedad, valor),
             y = valor)) +
  geom_col() +
  geom_text(aes(label = prettyNum(round(valor, 2),big.mark = ",")),
            hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0))


# Todas las entidades: ----
entidades <- bd2 %>%
  filter(nivel == 2) %>%
  filter(year == 2021) %>%
  group_by(Entidad, year) %>%
  mutate(lugar = rank(-valor)) %>%
  # filter(lugar <= 20) %>%
  arrange(lugar)

N1 <- bd2 %>%
  filter(nivel == 1)

cat_n1 <- N1 %>%
  select(Enfermedad, clave) %>%
  unique()

# Catálogo de causas: ----

# Todas las muertes
todas <- bd2 %>%
  filter(Enfermedad == "Total") %>%
  select(-clave) %>%
  rename(total_defunciones = valor) %>%
  select(CVE_ENT, year, total_defunciones)

# Proyección CONAPO de población ----
proy <- readxl::read_xlsx("01_Datos/proyeccion_poblacion_conapo.xlsx") %>%
  select(-no) %>%
  rename(proy_poblacion  = valor)

# Base maestra: ----
bm <- entidades %>%
  ungroup() %>%
  left_join(todas) %>%
  rename(cve_ent = CVE_ENT) %>%
  left_join(proy) %>%
  mutate(pp = 100*(valor/total_defunciones),
         tasa_10000 = valor/(proy_poblacion/10000)) %>%
  mutate(tipo_enfermedad = case_when(clave == "06T"~ "COVID-19",
                                     clave == "20D"~ "Diabetes mellitus",
                                     clave == "E54"~ "Suicidios",
                                     str_detect(clave, pattern = "28|29|27")~ "Enfermedades del corazón",
                                     str_detect(clave, pattern = "33")~ "Enfermedades transmisibles",
                                     str_detect(clave, pattern = "35|30|38|48")~ "Otras enfermedades no transmisibles",
                                     str_detect(clave, pattern = "49|51")~ "Accidentes",
                                     str_detect(clave, pattern = "55")~ "Agresiones (homicidios)",
                                     str_detect(clave, pattern = "54")~ "Suicidios",
                                     str_detect(clave, pattern = "08|09|10|11|12|13|14|15|16|17|18")~ "Tumores"
                                     ))

bm_tum <- bm %>%
  filter(tipo_enfermedad == "Tumores") %>%
  group_by(cve_ent, Entidad, year) %>%
  summarise(valor = sum(valor, na.rm = T),
            clave = "Tumores",
            nivel = "2",
            lugar = 20,
            total_defunciones = total_defunciones,
            proy_poblacion = proy_poblacion,
            tasa_10000 = total_defunciones/(proy_poblacion/10000),
            tipo_enfermedad = "Tumores",
            Enfermedad = "Tumores") %>%
  unique() %>%
  mutate(pp = 100*(valor/total_defunciones)) %>%
  select(names(bm))


bm <- rbind(bm, bm_tum)

vec_colores <- c("Otras enfermedades no transmisibles" = "#5CC9FA",
                 "Enfermedades del corazón" = "#EFA9A1",
                 "Diabetes mellitus" = "#FBE7A3",
                 "Tumores" = "#F3DDF0",
                 "Accidentes" = "#D2A687",
                 "Agresiones (homicidios)" = "#Ea3323",
                 "Enfermedades transmisibles" = "#CADFB8",
                 "COVID-19" = "#4f6c35",
                 "Suicidios" = "purple")


# Gráfica 01: Comparación entre las distribuciones Nacionales y Coahuila.
datos_g1 <- bm %>%
  filter(cve_ent %in% c(
    # "00",
                        "05"
                        )) %>%
  filter(lugar <= 20) %>%
  mutate(just = ifelse(valor > 1000, 1, -0.1)) %>%
  mutate(color = ifelse((tipo_enfermedad %in% c("Agresiones (homicidios)", "COVID-19") & just == 1),
                        yes = "white",
                        no = "black"))

datos_g1 %>%
  ggplot(aes(x = reorder(str_wrap(Enfermedad, 40), valor),
             y = valor,
             fill = tipo_enfermedad)) +
  geom_col() +
  geom_text(aes(label = str_c(
    prettyNum(round(valor,2), big.mark = ","),
    " ",
    "(", round(pp, 2), "%)"
  )),
  hjust = datos_g1$just,
  color = datos_g1$color,
  fontface = "bold",
  family = "Mulish") +
  labs(x = NULL, y = NULL,
       fill = "Tipo de enfermedad",
       title = "Top-20 Coahuila") +
  coord_flip() +
  scale_fill_manual(values = vec_colores) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  # facet_wrap(~Entidad,
  #            scales = "free")  +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        axis.text.y = element_text(angle = 0, family = "Mulish", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("03_Visualizaciones/coahuila2_top_20.png",
       device = "png",
       height = 8,
       width = 9)

datos_g1 <- bm %>%
  filter(cve_ent %in% c(
    "00"
    # ,
    # "05"
  )) %>%
  filter(lugar <= 20) %>%
  mutate(just = ifelse(valor > 50000, 1, -0.1)) %>%
  mutate(color = ifelse((tipo_enfermedad %in% c("Agresiones (homicidios)", "COVID-19") & just == 1),
                        yes = "white",
                        no = "black"))

datos_g1 %>%
  ggplot(aes(x = reorder(str_wrap(Enfermedad, 40), valor),
             y = valor,
             fill = tipo_enfermedad)) +
  geom_col() +
  geom_text(aes(label = str_c(
    prettyNum(round(valor,2), big.mark = ","),
    " ",
    "(", round(pp, 2), "%)"
  )),
  hjust = datos_g1$just,
  color = datos_g1$color,
  fontface = "bold",
  family = "Mulish") +
  labs(x = NULL, y = NULL,
       fill = "Tipo de enfermedad",
       title = "Top-20 Nacional") +
  coord_flip() +
  scale_fill_manual(values = vec_colores) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        axis.text.y = element_text(angle = 0, family = "Mulish", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("03_Visualizaciones/nacional_top_20.png",
       device = "png",
       height = 8,
       width = 9)

# evolución:
total <- bd2 %>%
  filter(nivel == 2) %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  arrange(-valor) %>%
  mutate(tipo_enfermedad = case_when(clave == "06T"~ "COVID-19",
                                     clave == "20D"~ "Diabetes mellitus",
                                     clave == "E54"~ "Suicidios",
                                     str_detect(clave, pattern = "28|29|27")~ "Enfermedades del corazón",
                                     str_detect(clave, pattern = "33")~ "Enfermedades transmisibles",
                                     str_detect(clave, pattern = "35|30|38|48")~ "Otras enfermedades no transmisibles",
                                     str_detect(clave, pattern = "49|51")~ "Accidentes",
                                     str_detect(clave, pattern = "55")~ "Agresiones (homicidios)",
                                     str_detect(clave, pattern = "54")~ "Suicidios",
                                     str_detect(clave, pattern = "08|09|10|11|12|13|14|15|16|17|18")~ "Tumores"))

evo <- total %>%
  left_join(todas) %>%
  rename(cve_ent = CVE_ENT) %>%
  left_join(proy) %>%
  group_by(cve_ent, Entidad, tipo_enfermedad, year) %>%
  summarise(valor = sum(valor, na.rm = T),
          # clave = "Tumores",
          # nivel = "2",
          # lugar = 20,
          total_defunciones = total_defunciones,
          proy_poblacion = proy_poblacion,
          tasa_10000 = total_defunciones/(proy_poblacion/10000))

evo %>%
  mutate(tipo_enfermedad = ifelse(is.na(tipo_enfermedad),
                                  yes = "Otras",
                                  no = tipo_enfermedad)) %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = year,
             y = valor,
             group = str_wrap(tipo_enfermedad, 20),
             color = tipo_enfermedad ,
             fill = tipo_enfermedad
             )) +
  geom_line(size = 2) +
  geom_point(pch = 21, color = "black", size = 2) +
  labs(y = NULL, x = NULL,
       title = "Evolución de las principales causas\nde fallecimiento en Coahuila",
       caption = "Elaboración propia con datos de INEGI 2021.
       Tabulado de mortalidad: Estado de residencia X Lista mexicana de enfermedades.") +
  scale_x_continuous(breaks = seq(2011, 2021, 2)) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0),
                     labels = scales::comma_format()) +
  scale_color_manual(values = vec_colores) +
  scale_fill_manual(values = vec_colores) +
  facet_wrap(~str_wrap(tipo_enfermedad, 20),
             scales = "free_x") +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(angle = 0, family = "Mulish", vjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 90, family = "Mulish", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(family = "Mulish", face = "bold"),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

ggsave("03_Visualizaciones/evolucion_causas.png",
       device = "png",
       height = 7,
       width = 6)




