# Librerias ----
library(tidyverse)

# 1. Datos ----
catalogo <- tibble::tribble(
              ~cve_ent,              ~entidad,
                  "00",            "Nacional",
                  "01",      "Aguascalientes",
                  "02",     "Baja California",
                  "03", "Baja California Sur",
                  "04",            "Campeche",
                  "05",            "Coahuila de Zaragoza",
                  "06",              "Colima",
                  "07",             "Chiapas",
                  "08",           "Chihuahua",
                  "09",    "Ciudad de México",
                  "10",             "Durango",
                  "11",          "Guanajuato",
                  "12",            "Guerrero",
                  "13",             "Hidalgo",
                  "14",             "Jalisco",
                  "15",              "México",
                  "16",           "Michoacán de Ocampo" ,
                  "17",             "Morelos",
                  "18",             "Nayarit",
                  "19",          "Nuevo León",
                  "20",              "Oaxaca",
                  "21",              "Puebla",
                  "22",           "Querétaro",
                  "23",        "Quintana Roo",
                  "24",     "San Luis Potosí",
                  "25",             "Sinaloa",
                  "26",              "Sonora",
                  "27",             "Tabasco",
                  "28",          "Tamaulipas",
                  "29",            "Tlaxcala",
                  "30",            "Veracruz de Ignacio de la Llave",
                  "31",             "Yucatán",
                  "32",           "Zacatecas"
              )

hom <- readxl::read_xlsx("01_Datos/INEGI homicidios estados.xlsx") %>%
  mutate(across(.cols = 2:ncol(.), .fns = function(x){str_remove_all(x, pattern = ",") %>% as.numeric()})) %>%
  pivot_longer(cols = 3:ncol(.),
               names_to = "Entidad",
               values_to = "Homicidios") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  mutate(Porcentaje = 100*(Homicidios/sum(Homicidios))) %>%
  arrange(year, -Porcentaje) %>%
  mutate(rank = rank(-Porcentaje, ties.method = "first"))
hom_muni <- readxl::read_xlsx("01_Datos/INEGI homicidios con Municipios Coahuila.xlsx")
incidencia <- readxl::read_xlsx("01_Datos/INEGI incidencia delictiva.xlsx") %>%
  mutate(`2020` = as.numeric(`2020`)) %>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "year",
               values_to = "tasa de incidencia")
proyeccion <- readxl::read_xlsx("01_Datos/proyeccion_poblacion_conapo.xlsx")

# Gráfica de incidencia: ----
incidencia %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  ggplot(aes(x = year,
             y = `tasa de incidencia`,
             group = Entidad)) +
  geom_point() +
  geom_line()

# incidencia %>%
#   filter(year == max(year))




# Grafica de lugares: ----
hom %>%
  filter(year >= 2010) %>%
  mutate(is.coahuila = ifelse(Entidad == "Coahuila de Zaragoza",
                              yes = T, no = F)) %>%
  ggplot(aes(x = year, y = rank, group = Entidad, color = is.coahuila)) +
  geom_line() +
  scale_x_continuous(breaks = 1990:2021)

# Grafica de evolución Coahuila: ----
hom_pc <- hom %>%
  filter(year >= 2010) %>%
  left_join(catalogo, by = c("Entidad" = "entidad")) %>%
  left_join(proyeccion) %>%
  mutate(hom_pc_10000 = Homicidios/(valor/10000)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank_pc = rank(-hom_pc_10000),
         rank_pc_inverso = rank(hom_pc_10000))

ultimo_año <- hom_pc %>%
  filter(year == 2021)

ultimo_año %>%
  ggplot(aes(x = reorder(Entidad, hom_pc_10000), y = hom_pc_10000)) +
  geom_col(fill = "#780101") +
  geom_text(aes(label = prettyNum(round(hom_pc_10000, 2))),
            size = 5,
            family = "Impact",
            hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +

  labs(title = "Homicidios por cada 10,000 habitantes",
       y = NULL, x = NULL,
       subtitle = "Datos de Homicidios de INEGI para 2021",
       caption = "INEGI. Homicidios por entidad federativa.\nConsultado en\nhttps://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/defuncioneshom.asp?s=est\nProyección de población a medio año de CONAPO") +
  theme_bw() +
  theme(text = element_text(family = "Impact"),
        axis.text.y = element_text(angle = 0, family = "Impact", vjust = 0.5, size = 10),
        plot.title = element_text(family = "Impact", size = 18),
        plot.subtitle = element_text(family = "Impact", size = 12),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

ggsave("03_Visualizaciones/Homicidios_pc_coahuila_2021.png",
       height = 7, width = 6)


# Evolución de los Homicidios per cápita: ----
hom_pc %>%
  ggplot(aes(x = year,
             y = hom_pc_10000)) +
  geom_line() +
  facet_wrap(~Entidad)


lugar_evolucion %>%
  group_by(Entidad) %>%
  summarise(delta = max(rank_pc) - min(rank_pc)) %>%
  arrange(-delta)

lugar_evolucion <- hom_pc %>%
  mutate(is.coahuila = ifelse(Entidad == "Coahuila de Zaragoza",
                              yes = T, no = F))

linea_a <- lugar_evolucion %>%
  filter(Entidad == "Coahuila de Zaragoza")

linea_b <- lugar_evolucion %>%
  filter(Entidad != "Coahuila de Zaragoza")

etiquetas <- lugar_evolucion %>%
  filter(year %in% c(2010, 2021)) %>%
  select(year, Entidad, rank_pc_inverso) %>%
  mutate(year = ifelse(year == 2010,
                       yes = year - 0.3,
                       no = year + 0.3),
         hjust_i = ifelse(year == 2010,
                       yes = 1,
                       no = 0)) %>%
  mutate(is.coahuila = ifelse(Entidad == "Coahuila de Zaragoza",
                              yes = T, no = F))


linea_b %>%
  ggplot(aes(x = year, y = rank_pc_inverso,
             group = Entidad)) +
  geom_line(color = "gray") +
  geom_line(data = linea_a, aes(x = year, y = rank_pc_inverso,
                                group = Entidad),
            color = "#780101", size = 3) +
  geom_label(aes(label = rank_pc),
             color = "gray") +
  geom_label(data = linea_a, aes(x = year, y = rank_pc_inverso, label = rank_pc),
             color = "#780101", size = 6, fontface = "bold") +
  geom_text(data = etiquetas, aes(x = year,
                                  y = rank_pc_inverso,
                                  color = is.coahuila,
                                  label = Entidad,
                                  size = is.coahuila,
                                  hjust = factor(year)),
            family = "Impact") +
  scale_x_continuous(breaks = 2010:2021, limits = c(2007, 2024)) +
  scale_discrete_manual(aesthetics = "hjust", values = c(1,0), guide = "none") +
  scale_color_manual(values = c("gray50", "#780101"), guide = "none") +
  scale_size_manual(values = c(5,7), guide = "none") +
  labs(title = "Evolución en los lugares de Homicidio por cada 10,000 habitantes en el periodo 2010 a 2021",
       y = "Lugar", x = NULL, caption = "Elaboración propia con datos de Homicidios de INEGI y las proyecciones de población de CONAPO.") +
  theme_bw() +
  theme(text = element_text(family = "Impact"),
        axis.text.x = element_text(angle = 90, family = "Impact", vjust = 0.5, size = 15),
        plot.title = element_text(family = "Impact", size = 25),
        plot.subtitle = element_text(family = "Impact", size = 12),
        plot.caption = element_text(family = "Impact", size = 12),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

ggsave("03_Visualizaciones/evolucion_lugar_coahuila.png",
       height = 13,
       width = 14)


hom_pc %>%
  filter(rank_pc <= 5) %>%
  ggplot(aes(x = year, y = rank_pc, group = Entidad, color = Entidad)) +
  geom_line()

# Evolución de los homicidios a nivel nacional:
hom %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  filter(year >= 2005) %>%
  ggplot(aes(x = year,
             y = Homicidios)) +
  geom_col(fill = "#780101") +
  geom_text(aes(label = Homicidios),
            angle = 90,
            hjust = 1.2,
            size = 4,
            color = "white",
            family = "Impact") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  scale_x_continuous(breaks = 2005:2021) +
  labs(title = "Homicidios totales - Estado de Coahuila",
       y = NULL, x = NULL,
       subtitle = "Datos de Homicidios de INEGI 2005-2021",
       caption = "INEGI. Homicidios por entidad federativa.\nConsultado en\nhttps://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/defuncioneshom.asp?s=est") +
  theme_bw() +
  theme(text = element_text(family = "Impact"),
        axis.text.x = element_text(angle = 90, family = "Impact", vjust = 0.5),
        plot.title = element_text(family = "Impact", size = 18),
        plot.subtitle = element_text(family = "Impact", size = 12),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

ggsave("03_Visualizaciones/Homicidios_totales_coahuila.png",
       height = 7, width = 6)


hom %>%
  filter(year >= 2018) %>%
  ggplot(aes(x = year,
             y = Homicidios)) +
  geom_col() +
  facet_wrap(~Entidad)





