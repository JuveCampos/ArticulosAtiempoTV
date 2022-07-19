library(tidyverse)
library(zoo)
library(ggtext)
library(lubridate)
library(cowplot)
library(ggridges)

# Exceso de mortalidad ----
exceso <- readxl::read_xlsx("01_Datos/Exceso Mortalidad.xlsx")

e = exceso %>%
  mutate(SE = str_c(Anio,"-", SE),
         SE = factor(SE, levels = str_c(unique(SE)))) %>%
  pivot_longer(cols = 2:3) %>%
  mutate(SE_2 = str_extract(SE, pattern = "\\-\\d+") %>% str_remove_all(pattern = "\\-") %>% as.numeric()) %>%
  arrange(Anio, SE_2) %>%
  mutate(label = ifelse(SE_2 %in% c(2,10,20,30,40,52),
                        yes = SE_2,
                        no = NA))

# e_numerica


e$SE
e2 <- e %>%
  filter(SE == "2022-13")

e2$value <- c(e2$value[1] + 30,
              e2$value[1] - 60)


e %>%
  mutate(name = factor(name, levels = c("Observadas",
                                        "Esperadas"))) %>%
  ggplot(aes(x = SE,
             y = value,
             group = name,
             color = name,
             fill = name)) +
  geom_vline(xintercept = c("2020-1",
                            "2021-1",
                            "2022-1"),
             color = "gray50") +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_density_line(stat = "identity", size = 1,
                    alpha = 0.3)  +
  geom_text(aes(label = label,
                y = -60,
                x = SE),
            color = "black",
            fontface = "bold") +
  geom_text(data = e2, aes(x =SE,
                           y = value,
                           label = name,
                           color = name),
            hjust = 0,
            fontface = "bold",
            size = 6) +
  labs(y = "Defunciones",
       x = NULL,
       title = "¿Cómo evolucionó el exceso de mortalidad en el estado?",
       subtitle = "Evolución del Exceso de Mortalidad en Coahuila",
       caption = "Fuente: Exceso de Mortalidad en México\nhttps://coronavirus.gob.mx/exceso-de-mortalidad-en-mexico/"
  ) +
  scale_y_continuous(limits = c(-100,1500),
                     label = scales::comma_format()) +
  scale_x_discrete(expand = expansion(c(0, 0.15), 0)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish", hjust = 0.5,
                                         size = 15),
        plot.title = element_text(family = "Mulish", face = "bold",
                                  size = 20, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, family = "Mulish"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face = "bold", family = "Mulish", size = 18),
        plot.caption = element_text(face = "bold", family = "Mulish"),
        panel.grid.major = element_blank(),
        legend.position = "none")

ggsave("03_Visualizaciones/exceso_mortalidad_coahuila.png",
       width = 11,
       height = 7,
       device = "png")

e_numerica <- e %>%
  group_by(Anio, name) %>%
  summarise(total = sum(value, na.rm = T)) %>%
  mutate(diff = diff(total),
         sum = sum(total)) %>%
  mutate(pp = 100*(diff/sum))

e_numerica %>%
  ggplot(aes(x = name,
             y = total,
             fill = name)) +
  geom_col() +
  geom_text(aes(label = prettyNum(total, big.mark = ",")),
            family = "Mulish",
            hjust = 0.5,
            vjust = -0.3,
            fontface = "bold") +
  labs(title = "Exceso de mortalidad en Coahuila por año",
       subtitle = "Diferencia entre <b style = 'color:#01bfc4;'>defunciones observadas</b> y <b style = 'color:#F8766d;'>defunciones esperadas</b><br>para el estado de Coahuila",
       x = NULL, y = "Fallecimientos") +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(c(0, 0.2), 0)) +
  facet_wrap(~Anio) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.subtitle = element_markdown(family = "Mulish", hjust = 0.5, size = 14),
        plot.title = element_text(family = "Mulish", face = "bold",
                                  size = 18, hjust = 0.5),
        plot.title.position = "plot",
        axis.text.y = element_text(size = 12, family = "Mulish"),
        axis.text.x = element_text(size = 12, family = "Mulish"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face = "bold", family = "Mulish", size = 18),
        plot.caption = element_text(face = "bold", family = "Mulish"),
        panel.grid.major = element_blank(),
        strip.text = element_text(family = "Mulish", size = 14, face = "bold"),
        legend.position = "none")

ggsave("03_Visualizaciones/resumen_exceso_mortalidad.png")
