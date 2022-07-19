# Librerias ----
library(tidyverse)

# Datos ----

# Shapes Secciones:

col_partidos <- tribble(
  ~Partido, ~Color,
  "PAN", "#0049d1",
  "PRI", "#de0f00",
  "PRD", "#ded300",
  "MC", "#de8d00",
  "PVEM", "#00de55",
  "MORENA", "#a30000",
  "NA", "#02ada2",
  "PANAL", "#02ada2",
  "PES" , "purple",
  "INDEPENDIENTE", "#b3009b",
  "PT", "#b33c00",
  "SIN VOTOS", "gray")
# %>%
#   filter(Partido %in% pop$value)

secciones <- readRDS("01_Datos/shape.rds") %>%
  rename(SECCION = seccion)

municipios <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")

# secciones$seccion[1:4]

# AGUASCALIENTES: ----

names(ags)[str_detect(names(ags), pattern = "PT|PVEM")] %>%
  str_c(collapse = " + ") %>%
  writeLines()

ags <- read_csv("01_Datos/20220606_0129_PREP_GUB_AGS/AGS_GUB_2022.csv",
                skip = 4)


ags <- read_csv("01_Datos/20220606_0129_PREP_GUB_AGS/AGS_GUB_2022.csv",
                  skip = 4) %>%
  mutate(across(.cols = c(1:3, 5, 7, 8),
                .fns = function(x){str_remove_all(x, pattern = '\\=|\\"')})) %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
                .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN_PRI_PRD = PAN + PRI + PRD + CO_PAN_PRI_PRD + CO_PAN_PRI + CO_PAN_PRD + CO_PRI_PRD,
         votos_MORENA = MORENA,
         votos_FXM= FXM,
         votos_PT_PVEM = PVEM + PT + CO_PVEM_PT,
         votos_MC = MC) %>%
  group_by(SECCION, ID_ENTIDAD) %>%
  summarise(votos_PAN_PRI_PRD = sum(votos_PAN_PRI_PRD, na.rm = T),
            votos_PT_PVEM = sum(votos_PT_PVEM, na.rm = T),
            votos_MORENA = sum(votos_MORENA, na.rm = T),
            votos_FXM = sum(votos_FXM, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T),
            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))



ganador_ags <- ags %>%
  select(-ID_ENTIDAD) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:6,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC") %>%
           str_replace(pattern = "Coalición FXM", replacement = "FXM") %>%
           str_replace(pattern = "Coalición MORENA", replacement = "MORENA")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(ags$ID_ENTIDAD[1]))

mapa_ags <- left_join(secciones %>%
                          filter(entidad == ags$ID_ENTIDAD[1]),
                        ganador_ags, by = c("SECCION", "entidad"))

mpios_ags <- municipios %>%
  filter(CVE_ENT == ags$ID_ENTIDAD[1])

mapa_ags %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = mpios_ags, fill = NA, color = "white",
          size = 1) +
  scale_fill_manual(values = c("blue", # MORENA
                               "brown", #PAN
                               "gray50" # NA
  )) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron en Aguascalientes en las elecciones de 2022?",
       caption = "Fuente: PREP Aguascalientes") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 2))

ggsave("03_Visualizaciones/ags_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)

# TAMAULIPAS: ----

names(tamps)[str_detect(names(tamps), pattern = "MOR")] %>%
  str_c(collapse = " + ") %>%
  writeLines()

tamps <- read_csv("01_Datos/20220606_0800_PREP_GUB_TAMPS/TAMPS_GUB_2022.csv",
         skip = 4) %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
                .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN_PRI_PRD = PAN + PRI + C_PAN_PRI_PRD + C_PAN_PRI + C_PAN_PRD + C_PRI_PRD,
         votos_MORENA_PT_PVEM = `Juntos Hacemos Historia en Tamaulipas`,
         votos_MC = MC) %>%
  group_by(SECCION, ID_ENTIDAD) %>%
  summarise(votos_PAN_PRI_PRD = sum(votos_PAN_PRI_PRD, na.rm = T),
            votos_MORENA_PT_PVEM = sum(votos_MORENA_PT_PVEM, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T),
            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))

ganador_tamps <- tamps %>%
  select(-ID_ENTIDAD) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:4,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(tamps$ID_ENTIDAD[1]))

mapa_tamps <- left_join(secciones %>%
                          filter(entidad == tamps$ID_ENTIDAD[1]),
                      ganador_tamps, by = c("SECCION", "entidad"))

mpios_tamps <- municipios %>%
  filter(CVE_ENT == tamps$ID_ENTIDAD[1])

mapa_tamps %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = mpios_tamps, fill = NA, color = "white",
          size = 1) +
  scale_fill_manual(values = c("brown", # MORENA
                               "blue", #PAN
                               "gray50" # NA
  )) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron en Tamaulipas en las elecciones de 2022?",
       caption = "Fuente: PREP Tamaulipas") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 2))
ggsave("03_Visualizaciones/tamaulipas_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)

# HIDALGO:----

hgo <- read_csv("01_Datos/20220606_0937_PREP_GUB_HGO/HGO_GUB_2022.csv",
                skip = 4) %>%
  mutate(across(.cols = c(1:3, 5, 7, 8),
                .fns = function(x){str_remove_all(x, pattern = '\\=|\\"')}))  %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
                .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN_PRI_PRD = PAN + PRI + PRD + CO_PAN_PRI_PRD + CO_PAN_PRI + CO_PAN_PRD + CO_PRI_PRD,
         votos_MORENA_PT_NAH = PT + MORENA + NAH + CC_PT_MORENA_NAH + CC_PT_MORENA + CC_PT_NAH + CC_MORENA_NAH,
         votos_MC = MC
  ) %>%
  group_by(SECCION, ID_ENTIDAD) %>%
  summarise(votos_PAN_PRI_PRD = sum(votos_PAN_PRI_PRD, na.rm = T),
            votos_MORENA_PT_NAH = sum(votos_MORENA_PT_NAH, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T),

            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))

ganador_hgo <- hgo %>%
  select(-ID_ENTIDAD) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:4,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(hgo$ID_ENTIDAD[1]))

mapa_hgo <- left_join(secciones %>% filter(entidad == hgo$ID_ENTIDAD[1]),
                       ganador_hgo, by = c("SECCION", "entidad"))

mpios_hgo <- municipios %>%
  filter(CVE_ENT == hgo$ID_ENTIDAD[1])

mapa_hgo %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = mpios_hgo, fill = NA, color = "white",
          size = 1) +
  scale_fill_manual(values = c("brown", # MORENA
                               "blue", #PAN
                               "orange", #MC
                               "gray50" # NA
  )) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron los Hidalguenses en las elecciones de 2022?",
       caption = "Fuente: PREP Hidalgo") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 3))
ggsave("03_Visualizaciones/hidalgo_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)

# QUNTANA ROO: ----
names(qroo)[str_detect(names(qroo), pattern = "PRI")] %>%
  str_c(collapse = " + ") %>%
  writeLines()

qroo <- read_delim("01_Datos/20220606_1300_PREP/20220606_1300_PREP_GUB_QROO/QROO_GUB_2022.csv",
           delim = "|", escape_double = FALSE, trim_ws = TRUE,
           skip = 5) %>%
  mutate(across(.cols = c(1, 5, 7),
                .fns = function(x){str_remove_all(x, pattern = '\\=|\\"')}))  %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
                .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN_PRI_PRD_CQROO = PAN + PRI + PRD + CXQROO + C_PAN_PRD_CQROO + C_PAN_PRD + C_PAN_CQROO + C_PRD_CQROO,
         votos_MORENA_PT_PVEM_FXMQROO = PVEM + PT + MORENA + FXMQROO + C_PVEM_PT_MORENA_FXMQROO + C_PVEM_PT_MORENA + C_PVEM_PT_FXMQROO + C_PVEM_MORENA_FXMQROO + C_PT_MORENA_FXMQROO + C_PVEM_PT + C_PVEM_MORENA + C_PVEM_FXMQROO + C_PT_MORENA + C_PT_FXMQROO + C_MORENA_FXMQROO ,
         votos_MC = MC,
         votos_MAS = MAS
  ) %>%
  group_by(SECCION, ID_ESTADO) %>%
  summarise(votos_PAN_PRI_PRD_CQROO = sum(votos_PAN_PRI_PRD_CQROO, na.rm = T),
            votos_MORENA_PT_PVEM_FXMQROO = sum(votos_MORENA_PT_PVEM_FXMQROO, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T),
            votos_MAS = sum(votos_MAS, na.rm = T),
            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))

ganador_qroo <- qroo %>%
  select(-ID_ESTADO) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:5,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC") %>%
           str_replace(pattern = "Coalición MAS", replacement = "MAS")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(qroo$ID_ESTADO[1]))


mapa_qroo <- left_join(secciones %>% filter(entidad == qroo$ID_ESTADO[1]),
                      ganador_qroo, by = c("SECCION", "entidad"))

mpios_qroo <- municipios %>%
  filter(CVE_ENT == qroo$ID_ESTADO[1])

mapa_qroo %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = mpios_qroo, fill = NA, color = "white",
          size = 1) +
  scale_fill_manual(values = c("brown", # MORENA
                               "blue", #PAN
                               "purple", #MAS
                               "orange", #MC
                               "gray50" # NA
  )) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron los Quintanarroenses en las elecciones de 2022?",
       caption = "Fuente: PREP Quintana Roo") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 3))

ggsave("03_Visualizaciones/quintanaroo_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)

# OAXACA: ----

oax <- read_csv("01_Datos/20220606_1945_PREP_GUB_OAX/OAX_GUB_2022.csv",
         skip = 4) %>%
  mutate(SECCION = as.numeric(str_remove(SECCION, pattern = "\\'"))) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
                .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN = PAN,
         votos_PRI_PRD = PRI + PRD + C_PRI_PRD,
         votos_MORENA_PT_PVEM_PUP = MOR + PT + PVEM + PUP + C_PT_PV_PUP_MOR + C_PT_PV_MOR + C_PT_PUP_MOR + C_PV_PUP_MOR + C_PT_MOR + C_PV_MOR + C_PUP_MOR,
         votos_MC = MC,
         votos_NA = `NA`
         ) %>%
  group_by(SECCION, ID_ESTADO) %>%
  summarise(votos_PAN = sum(votos_PAN, na.rm = T),
            votos_PRI_PRD = sum(votos_PRI_PRD, na.rm = T),
            votos_MORENA_PT_PVEM_PUP = sum(votos_MORENA_PT_PVEM_PUP, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T),
            votos_NA = sum(votos_NA, na.rm = T),
            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))

ganador_oax <- oax %>%
  select(-ID_ESTADO) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:6,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC") %>%
           str_replace(pattern = "Coalición NA", replacement = "NA")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(oax$ID_ESTADO[1]))

mapa_oax <- left_join(secciones %>% filter(entidad == oax$ID_ESTADO[1]),
                      ganador_oax, by = c("SECCION", "entidad"))

mpios_oax <- municipios %>%
  filter(CVE_ENT == oax$ID_ESTADO[1])

mapa_oax %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0) +
  geom_sf(data = mpios_oax, fill = NA, color = "white",
          size = 0.1) +
  scale_fill_manual(values = c("brown", # MORENA
                               "blue", #PAN
                               "#de0f00", #PRI
                               "orange", #MC
                               "#02ada2", #NuevaAlianza
                               "gray50" # NA
                               )) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron los Oaxaqueños en las elecciones de 2022?",
       caption = "Fuente: PREP Oaxaca") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             ncol = 3))

ggsave("03_Visualizaciones/oaxaca_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)


# DURANGO: ----
dgo <- read_csv("01_Datos/20220606_2000_PREP_DGO/20220606_2000_PREP_GUB_DGO/DGO_GUB_2022.csv",
         skip = 4) %>%
  mutate(SECCION = as.numeric(SECCION)) %>%
  filter(TOTAL_PERSONAS_VOTARON != 0) %>%
  mutate(across(.cols = PAN:LISTA_NOMINAL,
         .fns = function(x){as.numeric(x)})) %>%
  mutate(votos_PAN_PRI_PRD = PAN + PRI + PRD + C_PAN_PRI_PRD + C_PAN_PRI + C_PAN_PRD + C_PRI_PRD,
         votos_MORENA_PT_PVEM_RSP = PVEM + PT + MORENA + RSP + C_PVEM_PT_MORENA_RSP + C_PVEM_PT_MORENA + C_PVEM_PT_RSP + C_PVEM_MORENA_RSP + C_PVEM_PT + C_PVEM_MORENA + C_PVEM_RSP + C_PT_MORENA_RSP + C_PT_MORENA + C_PT_RSP + C_MORENA_RSP ,
         votos_MC = MC) %>%
  group_by(SECCION, ID_ENTIDAD) %>%
  summarise(votos_PAN_PRI_PRD = sum(votos_PAN_PRI_PRD, na.rm = T),
            votos_MORENA_PT_PVEM_RSP = sum(votos_MORENA_PT_PVEM_RSP, na.rm = T),
            votos_MC = sum(votos_MC, na.rm = T ),
            NULOS = sum(NULOS, na.rm = T),
            TOTAL_VOTOS_CALCULADO = sum(TOTAL_VOTOS_CALCULADO, na.rm = TRUE),
            LISTA_NOMINAL = sum(LISTA_NOMINAL, na.rm = T)) %>%
  mutate(ABSTENCION = LISTA_NOMINAL - TOTAL_VOTOS_CALCULADO) %>%
  mutate(Participacion = 100*(TOTAL_VOTOS_CALCULADO/LISTA_NOMINAL))

ganador_dgo <- dgo %>%
  select(-ID_ENTIDAD) %>%
  select(-TOTAL_VOTOS_CALCULADO,
         -ABSTENCION,
         -NULOS) %>%
  pivot_longer(2:4,
               names_to = "Partido o coalición",
               values_to = "Votos") %>%
  group_by(SECCION) %>%
  mutate(rank = rank(-Votos, ties.method = "first")) %>%
  filter(rank %in% 1:2) %>%
  mutate(diff = abs(diff(Votos))) %>%
  mutate(diff_pp = 100*(diff/LISTA_NOMINAL)) %>%
  mutate(mayor_5 = ifelse(diff_pp > 5, yes = "Mayor a 5%", no = "Menor al 5%")) %>%
  mutate(`Partido o coalición` = str_c("Coalición ", str_replace_all(`Partido o coalición`, pattern = "_", replacement = "-")) %>%
           str_remove(pattern = "votos-") %>%
           str_replace(pattern = "Coalición MC", replacement = "MC")) %>%
  filter(rank == 1) %>%
  mutate(Victoria = str_c(`Partido o coalición` , " ", mayor_5)) %>%
  ungroup() %>%
  select(SECCION, Participacion, `Partido o coalición`, mayor_5, Victoria) %>%
  mutate(entidad = as.character(dgo$ID_ENTIDAD[1]))

mapa_dgo <- left_join(secciones %>% filter(entidad == dgo$ID_ENTIDAD[1]),
                      ganador_dgo, by = c("SECCION", "entidad"))

mpios_dgo <- municipios %>%
  filter(CVE_ENT == dgo$ID_ENTIDAD[1])

mapa_dgo %>%
  ggplot(aes(fill = `Partido o coalición`)) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = mpios_dgo, fill = NA, color = "white", size = 1) +
  scale_fill_manual(values = c("#a30000",
                               "#3f8f56",
                               "#de8d00",
                               "gray90")) +
  labs(subtitle = "Partido Ganador por sección electoral",
       x = NULL, y = NULL, fill = "Partido",
       title = "¿Cómo votaron los duranguenses en las elecciones de 2022?",
       caption = "Fuente: PREP Durango.") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        strip.text = element_markdown(family = "Mulish"),
        plot.title = element_text(family = "Mulish", hjust = 0.5,
                                  face = "bold", size = 18),
        plot.caption = element_text(size = 10),
        plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 15),
        plot.title.position = "plot") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))


ggsave("03_Visualizaciones/durango_mpios.png",
       device = "png",
       height = 7.5, width = 7.5)

# ganador
# unique(ganador$`Partido o coalición`)
names(dgo)[str_detect(names(dgo), pattern = "MC")] %>%
  str_c(collapse = " + ") %>%
  writeLines()
