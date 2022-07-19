library(tidyverse)
library(zoo)
library(ggtext)
library(lubridate)
library(cowplot)

# datos ----

# Defunciones:
datos_defunciones <- read_csv("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Defunciones_20220706.csv")

# Confirmados:
datos_confirmados <- read_csv("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_20220706.csv")


# Procesamiento ----

confirmados <- datos_confirmados  %>%
  pivot_longer(4:ncol(.),
               names_to = "fecha",
               values_to = "casos") %>%
  mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

defunciones <- datos_defunciones %>%
  pivot_longer(4:ncol(.),
               names_to = "fecha",
               values_to = "defunciones") %>%
  mutate(fecha = as.Date(fecha, format = "%d-%m-%Y"))

edos <- defunciones %>%
  pull(nombre) %>%
  unique()

defunciones_edo <- defunciones %>%
  filter(nombre == "Nacional")

confirmados_edo <- confirmados %>%
  filter(nombre == "Nacional") %>%
  mutate(media_movil_7_dias = c(rep(0, 6),
                                rollmean(casos, 7)))

# Gráfica nacional:
confirmados_edo %>%
  ggplot(aes(x = fecha, y = casos)) +
  geom_col(fill = "gray90") +
  geom_line(aes(y = media_movil_7_dias),
            color = "#047a3b",
            size = 2) +
  labs(subtitle = "Casos confirmados de COVID-19 a nivel nacional<br><b style = 'color:#047a3b;'>Linea verde: </b>media móvil de 7 días.",
       title = "¿Cómo han evolucionado el número de contagios\nde COVID-19 en México?",
       caption = "Fuente: Tablero CONACYT COVID-19.\nhttps://datos.covid-19.conacyt.mx/#DownZCSV\nConsultado el 7 de julio del 2022",
       x = NULL, y = "Casos nuevos confirmados por día") +
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_date(
    breaks = "2 months") +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", face = "bold", size = 15),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("03_Visualizaciones/evolucion_casos_covid_nacional.png",
       height = 6,
       width = 6.5)


# Gráfica coahuila:
confirmados_edo <- confirmados %>%
  filter(nombre == "COAHUILA") %>%
  mutate(media_movil_7_dias = c(rep(0, 6),
                                rollmean(casos, 7)))

confirmados_edo %>%
  ggplot(aes(x = fecha, y = casos)) +
  geom_col(fill = "gray90") +
  geom_line(aes(y = media_movil_7_dias),
            color = "#047a3b",
            size = 2) +
  labs(subtitle = "Casos confirmados de COVID-19 a nivel estatal<br><b style = 'color:#047a3b;'>Linea verde: </b>media móvil de 7 días.",
       title = "¿Cómo han evolucionado el número de contagios\nde COVID-19 en el estado de Coahuila?",
       caption = "Fuente: Tablero CONACYT COVID-19.\nhttps://datos.covid-19.conacyt.mx/#DownZCSV\nConsultado el 7 de julio del 2022",
       x = NULL, y = "Casos nuevos confirmados por día") +
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_date(
    breaks = "2 months") +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", face = "bold", size = 15),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("03_Visualizaciones/evolucion_casos_covid_coahuila.png",
       height = 6,
       width = 6.5)

# Fallecimientos por cada 10.000 habitantes.
def_mil = defunciones %>%
  filter(nombre == "COAHUILA") %>%
  mutate(mes = month(fecha), year = year(fecha)) %>%
  group_by(mes, year) %>%
  summarise(total_fallecimientos = sum(defunciones)) %>%
  arrange(year, mes) %>%
  mutate(fecha = as.Date(str_c("01", "-", mes, "-", year),
                         format = "%d-%m-%Y"))
# %>%
#   mutate(tiempo = str_c(year, "-", mes)) %>%
#   mutate(tiempo = factor(tiempo, levels = unique(tiempo)))
# def_mil %>%
#   ggplot(aes(x = fecha, y = total_fallecimientos)) +
#   geom_col()

# Fallecimientos contra casos
defunciones_edo <- defunciones %>%
  filter(nombre == "COAHUILA")

fall_cas <- left_join(confirmados_edo, defunciones_edo) %>%
  select(-media_movil_7_dias) %>%
  pivot_longer(cols = 5:6) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  mutate(media_movil_7_dias = c(rep(0, 6),
                                rollmean(value, 7))) %>%
  mutate(mes = month(fecha), year = year(fecha)) %>%
  group_by(name, mes, year) %>%
  summarise(total_mensual = sum(value)) %>%
  arrange(year, mes) %>%
  mutate(fecha = as.Date(str_c("01", "-", mes, "-", year),
                         format = "%d-%m-%Y"))

fall_cas %>%
  ggplot(aes(x = fecha,
             y = total_mensual,
             fill = name)) +
  geom_col()


# Defunciones a nivel nacional
defunciones_edo <- defunciones %>%
  filter(nombre == "Nacional")

sum(defunciones_edo$defunciones)
defunciones_edo %>%
  mutate(year = year(fecha)) %>%
  group_by(year) %>%
  summarise(defunciones = sum(defunciones)) %>%
  ungroup() %>%
  mutate(pp = defunciones/sum(defunciones))


defunciones_edo <- defunciones %>%
  filter(nombre == "COAHUILA")

sum(defunciones_edo$defunciones)
defunciones_edo %>%
  mutate(year = year(fecha)) %>%
  group_by(year) %>%
  summarise(defunciones = sum(defunciones)) %>%
  ungroup() %>%
  mutate(pp = defunciones/sum(defunciones))

# Defunciones_por_entidad:

tasa <- defunciones %>%
  mutate(year = year(fecha)) %>%
  group_by(nombre, year) %>%
  summarise(defunciones = sum(defunciones),
            poblacion = first(poblacion)) %>%
  mutate(tasa_por_cada_10000 = defunciones/(poblacion/10000)) %>%
  mutate(barra_dif = case_when(nombre == "Nacional" ~ "Nacional",
                               nombre == "COAHUILA" ~ "COAHUILA",
                               TRUE ~ "Otros estados")) %>%
  mutate(expansion = ifelse(tasa_por_cada_10000 < 10,
                            yes = -0.1,
                            no =  1)) %>%
  mutate(nombre = str_replace_all(nombre, c("DISTRITO FEDERAL" = "CIUDAD DE MÉXICO")))

plots[[1]]

plots <- lapply(2020:2022, function(y){

plt <- tasa %>%
  filter(year == y) %>%
  ggplot(aes(x = reorder(nombre, tasa_por_cada_10000),
             y = tasa_por_cada_10000,
             fill = barra_dif)) +
  geom_col() +
  geom_text(aes(label = format(round(tasa_por_cada_10000,2), nsmall = 2),
                hjust = expansion),
            family = "Mulish", color = "gray10") +
  coord_flip() +
  scale_fill_manual(values = c("#538c6e", "gray50", "gray70"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  labs(title = y,
       y = "Fallecimientos por COVID por cada 10,000 habitantes",
       x = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", face = "bold",
                                  size = 15, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 0, hjust = 1))
return(plt)

})

?plot_grid
plot_grid(plots[[1]], plots[[2]], plots[[3]],
          align = "h", ncol = 3)

ggsave("03_Visualizaciones/tasa_entidades.png",
       height = 6.5,
       width = 11)



# Gráfica de fallecimientos por etapa de la pandemia ----
unique(defunciones$nombre)

pp_def <- defunciones %>%
  filter(nombre %in% c("Nacional", "COAHUILA")) %>%
  mutate(anio = year(fecha)) %>%
  group_by(anio, cve_ent, nombre) %>%
  summarise(defunciones = sum(defunciones),
            poblacion = first(poblacion)) %>%
  ungroup() %>%
  group_by(nombre) %>%
  mutate(pp = 100*(defunciones/sum(defunciones))) %>%
  mutate(is.2022 = ifelse(anio == 2022, yes = "2022", no = "no_2022")) %>%
  mutate(nombre = str_to_upper(nombre))

pp_def %>%
  ggplot(aes(x = anio, y = pp, alpha = is.2022)) +
  geom_col(fill = "#047a3b") +
  geom_richtext(aes(label = str_c("<b>", prettyNum(defunciones, big.mark = ","), "</b><br>",
                              "(", format(round(pp, 2), nsmall = 2), "%)")),
            vjust = -0.2, family = "Mulish",
            size = 4, label.color = NA) +
  scale_y_continuous(expand = expansion(c(0, 0.3), 0)) +
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_wrap(~nombre) +
  labs(title = "¿Cuantos fallecimientos atribuídos al COVID-19 hubo por año\nen Coahuila y en el país?",
       subtitle = "Número y porcentaje de fallecimientos por año en Coahuila y su comparacíón a nivel nacional",
       caption = "Fuente: Tablero CONACYT COVID-19.\nhttps://datos.covid-19.conacyt.mx/#DownZCSV\nConsultado el 7 de julio del 2022",
       x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish", hjust = 0.5),
        plot.title = element_text(family = "Mulish", face = "bold",
                                  size = 15, hjust = 0.5),
        plot.title.position = "plot",
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Mulish", hjust = 0.5, size = 12),
        strip.text = element_text(family = "Mulish", size = 10, face = "bold"))

ggsave("03_Visualizaciones/porcantaje_covid_edo_nac.png",
       height = 6, width = 8, device = "png")

# Gráfica de fallecimientos per-cápita ----
per_capita <- defunciones %>%
  mutate(anio = year(fecha)) %>%
  group_by(cve_ent, nombre) %>%
  summarise(defunciones = sum(defunciones),
            poblacion = first(poblacion)) %>%
  mutate(pc = defunciones/(poblacion/10000)) %>%
  mutate(barra_dif = case_when(nombre == "Nacional" ~ "Nacional",
                               nombre == "COAHUILA" ~ "COAHUILA",
                               TRUE ~ "Otros estados")) %>%
  mutate(nombre = str_to_upper(nombre)) %>%
  mutate(nombre = str_replace_all(nombre, c("DISTRITO FEDERAL" = "CIUDAD DE MÉXICO"))) %>%
  mutate(expansion = ifelse(pc < 10,
                            yes = -0.1,
                            no =  1))

per_capita %>%
  ggplot(aes(x = reorder(nombre, pc),
             y = pc,
             fill = barra_dif)) +
  geom_col() +
  geom_text(aes(label = format(round(pc,2), nsmall = 2), hjust = expansion, color  = barra_dif),
            family = "Mulish", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("#538c6e", "gray50", "gray70"),
                    guide = "none") +
  scale_color_manual(values = c("white", "white", "gray20")) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  labs(y = NULL,
       title = "Fallecimientos por COVID por cada 10,000 habitantes",
       subtitle = "Fallecimientos sumados desde el inicio de la pandemia",
       caption = "Fuente: Tablero CONACYT COVID-19.\nhttps://datos.covid-19.conacyt.mx/#DownZCSV\nConsultado el 7 de julio del 2022",
       x = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish", hjust = 0.5),
        plot.title = element_text(family = "Mulish",
                                  face = "bold",
                                  size = 15,
                                  hjust = 0.5),
        legend.position = "none",
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        axis.text.x = element_blank())

ggsave("03_Visualizaciones/fallecimientos_totales_pc.png",
       device = "png",
       height = 6, width = 5.5)

# Gráfica de casos per cápita ----
per_capita <- confirmados %>%
  mutate(anio = year(fecha)) %>%
  group_by(cve_ent, nombre) %>%
  summarise(casos = sum(casos),
            poblacion = first(poblacion)) %>%
  mutate(pc = casos/(poblacion/10000)) %>%
  mutate(barra_dif = case_when(nombre == "Nacional" ~ "Nacional",
                               nombre == "COAHUILA" ~ "COAHUILA",
                               TRUE ~ "Otros estados")) %>%
  mutate(nombre = str_to_upper(nombre)) %>%
  mutate(nombre = str_replace_all(nombre, c("DISTRITO FEDERAL" = "CIUDAD DE MÉXICO"))) %>%
  mutate(expansion = ifelse(pc < 300,
                            yes = -0.1,
                            no =  1.1))

per_capita %>%
  ggplot(aes(x = reorder(nombre, pc),
             y = pc,
             fill = barra_dif)) +
  geom_col() +
  geom_text(aes(label = format(round(pc,2), nsmall = 2), hjust = expansion, color  = barra_dif),
            family = "Mulish", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("#538c6e", "gray50", "gray70"),
                    guide = "none") +
  scale_color_manual(values = c("white", "white", "gray20")) +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  labs(y = NULL,
       title = "Casos confirmados de COVID-19\npor cada 10,000 habitantes",
       subtitle = "Casos sumados desde el inicio de la pandemia",
       caption = "Fuente: Tablero CONACYT COVID-19.\nhttps://datos.covid-19.conacyt.mx/#DownZCSV\nConsultado el 7 de julio del 2022",
       x = NULL) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish", hjust = 0.5),
        plot.title = element_text(family = "Mulish",
                                  face = "bold",
                                  size = 15,
                                  hjust = 0.5),
        legend.position = "none",
        plot.title.position = "plot",
        axis.ticks = element_blank(),
        axis.text.x = element_blank())

ggsave("03_Visualizaciones/casos_totales_pc.png",
       device = "png",
       height = 6, width = 5.5)


# Misceláneos:
confirmados %>%
  filter(casos != 0) %>%
  group_by(nombre) %>%
  filter(fecha == min(fecha)) %>%
  ungroup() %>%
  mutate(rank = rank(fecha, ties.method = "first")) %>%
  View()

sum(confirmados_edo$casos)




