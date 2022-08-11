# Library
library(tidyverse)
library(sf)

# Datos:
proy_conapo_municipal <- readxl::read_xlsx("01_Datos/conapo_muni.xlsx") %>%
  select(-no, -ponderador) %>%
  rename(poblacion = valor)

unique(bd$cve_ent)

bd <- readxl::read_xlsx("01_Datos/INEGI_exporta_11_8_2022_14_45_51.xlsx") %>%
  pivot_longer(cols = 4:ncol(.)) %>%
  mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric(),
         name = as.numeric(name)) %>%
  mutate(cve_ent = str_remove(cve_ent, pattern = " ")) %>%
  mutate(cve_ent = ifelse(cve_ent == "05",
                          "05000",
                          cve_ent)) %>%
  filter(str_length(cve_ent) > 2) %>%
  rename(year = name,
         fallecimientos = value) %>%
  filter(Enfermedad == "Total") %>%
  rename(cve_mun = cve_ent)

mapa_muni <- left_join(bd, proy_conapo_municipal) %>%
  filter(!is.na(poblacion)) %>%
  filter(year == 2021) %>%
  mutate(tasa = fallecimientos/(poblacion/10000)) %>%
  rbind(tibble(cve_mun = "05000",
               Entidad = "Coahuila de Zaragoza",
               Enfermedad = "Total",
               year = 2021,
               fallecimientos = 1,
               poblacion = 1,
               tasa = 70)) %>%
  rbind(tibble(cve_mun = "00000",
               Entidad = "Promedio Nacional",
               Enfermedad = "Total",
               year = 2021,
               fallecimientos = 1,
               poblacion = 1,
               tasa = 86)) %>%
  mutate(is.edo = ifelse(cve_mun == "05000",
                         yes = "olivedrab",
                         no = "skyblue"),
        is.edo =  ifelse(cve_mun == "00000",
                  yes = "purple",
                  no = is.edo),
        is.edo2 = ifelse(is.edo %in% c("olivedrab" ,"purple"),
                         yes = is.edo,
                         no = "gray50"),
        )

mapa_muni %>%
  ggplot(aes(x = reorder(Entidad, tasa),
             y = tasa)) +
  geom_col(fill = mapa_muni$is.edo) +
  geom_text(aes(label = format(round(tasa, 1), nsmall = 1)),
            hjust = -0.1,
            color = mapa_muni$is.edo2,
            fontface = "bold") +
  labs(title = "Tasas de mortalidad a a nivel municipal - Coahuila
       Fallecimientos por cada 10,000 habitantes - 2021.",
       x = NULL, y = NULL,
       caption = "Fuente: Elaboración propia con datos de los Tabulados de Mortalidad de INEGI, 2021.
       Tabulado de Entidad y municipio de residencia.
       Ponderación de población con datos de proyección de CONAPO, con excepción de los promedios nacional y estatal,
       los cuales se tomaron del informe de mortalidad de INEGI del 2021. ") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  coord_flip() +
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

ggsave("03_Visualizaciones/tasa_municipios.png",
       device = "png",
       height = 9,
       width = 8)

# Causas local:
bd <- readxl::read_xlsx("01_Datos/INEGI_exporta_11_8_2022_14_45_51.xlsx") %>%
  pivot_longer(cols = 4:ncol(.)) %>%
  mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric(),
         name = as.numeric(name)) %>%
  mutate(cve_ent = str_remove(cve_ent, pattern = " ")) %>%
  mutate(cve_ent = ifelse(cve_ent == "05",
                          "05000",
                          cve_ent)) %>%
  filter(str_length(cve_ent) > 2) %>%
  rename(year = name,
         fallecimientos = value) %>%
  filter(year == 2021) %>%
  filter(!(Enfermedad %in% c("Total", "1985-1997", "1998-2021"))) %>%
  group_by(cve_ent, Entidad) %>%
  mutate(ranking = rank(-fallecimientos, ties.method = "first")) %>%
  filter(ranking == 1) %>%
  arrange(Entidad, ranking) %>%
  filter(!is.na(Entidad))

shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson")
shape <- shp %>%
  filter(CVE_ENT == "05") %>%
  rename(cve_ent = CVEGEO)

mapa <- left_join(shape, bd)

wesanderson::wes_palettes$Cavalcanti1 %>% scales::show_col()

mapa %>%
  ggplot(aes(fill = Enfermedad)) +
  geom_sf(color = "white") +
  labs(fill = "Tipo de Enfermedad") +
  scale_fill_manual(values = c( "#02401B","#D8B70A","#A2A475", "#81A88D", "#972D15")) +
  theme_bw() +
  theme(text = element_text(family = "Mulish"),
        plot.title = element_text(family = "Mulish", size = 18, hjust = 0.5),
        plot.subtitle = element_text(family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold",
                                    size = 15,
                                    family = "Mulish")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 1))

ggsave("03_Visualizaciones/mapa_municipios.png",
       device = "png",
       height = 9,
       width = 8)

