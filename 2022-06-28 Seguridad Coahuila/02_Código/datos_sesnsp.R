library(tidyverse)

sesnsp <- read_csv("/Volumes/Extreme\ SSD/DATASETS/SESNSP\ -\ Delincuencia/Municipal-Delitos-2015-2021_sep2021/IDEFC_NM_may22.csv",
                   locale = locale(encoding = "WINDOWS-1252"))

unique(sesnsp$Entidad) %>% sort()
unique(sesnsp$`Tipo de delito`) %>% sort()
unique(sesnsp$`Subtipo de delito`) %>% sort()

homicidios_dolosos_anio = sesnsp %>%
  # filter(Entidad == "Coahuila de Zaragoza" ) %>%
  filter(`Subtipo de delito` == "Homicidio doloso") %>%
  select(Anio, Clave_Ent, Entidad, 8:ncol(.)) %>%
  pivot_longer(cols = Enero:Diciembre) %>%
  group_by(Anio, Clave_Ent, Entidad) %>%
  summarise(total = sum(value, na.rm = T))

datos_coahuila <- sesnsp %>%
  # filter(`Subtipo de delito` %in% c("Secuestro", "Robo de vehículo automotor", "Robo a casa habitación" , "Feminicidio", "Narcomenudeo", "Lesiones dolosas", "Abuso sexual", "Violencia familiar")) %>%
  filter(Entidad == "Coahuila de Zaragoza") %>%
  pivot_longer(cols = Enero:Diciembre) %>%
  group_by(Anio, Clave_Ent, Entidad, `Subtipo de delito`) %>%
  summarise(total = sum(value, na.rm = T))



datos_coahuila %>%
  ggplot(aes(x = Anio, y = total)) +
  geom_col(fill = "#780101") +
  geom_text(aes(label = prettyNum(total, big.mark = ",")),
            vjust = -0.5,
            family = "Impact",
            color = "black") +
  scale_y_continuous(expand = expansion(c(0, 0.3), 0)) +
  scale_x_continuous(breaks = 2015:2022) +
  facet_wrap(~`Subtipo de delito`,
             ncol = 2,
             scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "¿Cómo han evolucionado las carpetas de investigación de delitos en Coahuila?",
       subtitle = "Evolución 2015-2020. Delitos seleccionados.",
       caption = "Datos del SESNSP contabilizados por año") +
  theme_bw() +
  theme(strip.text = element_text(family = "Impact", size = 15, color = "#780101"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, family = "Impact", vjust = 0.5, size = 14),
        plot.title = element_text(family = "Impact", size = 18),
        plot.subtitle = element_text(family = "Impact", size = 12),
        plot.caption = element_text(family = "Impact", size = 10),
        text = element_text(family = "Impact"),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        # panel.border = element_blank(),
        axis.ticks.y = element_blank()
        # ,
        # panel.grid = element_blank()
        )


ggsave("03_Visualizaciones/delitos_sesnsp_2022.png",
       height = 11,
       width = 9)




