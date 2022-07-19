# Población viviendo en sequia:
library(tidyverse)

# Datos;
pob = readxl::read_xlsx("Bases parciales/proyeccion_poblacion/proys_conapo_ent.xlsx")
pob_1 = read_csv("/Users/juvenalcampos/Documents/GitHub/indicadoresMunicipales/01_Datos/Proyecciones\ CONAPO/Proyecciones\ CONAPO/base_municipios_final_datos_01.csv",locale = locale(encoding = "WINDOWS-1252"))
pob_2 = read_csv("/Users/juvenalcampos/Documents/GitHub/indicadoresMunicipales/01_Datos/Proyecciones\ CONAPO/Proyecciones\ CONAPO/base_municipios_final_datos_02.csv",locale = locale(encoding = "WINDOWS-1252"))

pob = rbind(pob_1, pob_2) %>%
  mutate(CLAVE = ifelse(str_length(CLAVE) == 4,
                        yes = str_c("0", CLAVE),
                        no = CLAVE))

pob_muni = pob %>%
  group_by(CLAVE,CLAVE_ENT, MUN, ANIO) %>%
  summarise(POB = sum(POB)) %>%
  ungroup() %>%
  select(-MUN) %>%
  mutate(CLAVE_ENT = ifelse(str_length(CLAVE_ENT) == 1,
                            yes = str_c("0", CLAVE_ENT),
                            no = CLAVE_ENT))

pob_ent = pob_muni %>%
  group_by(CLAVE_ENT, ANIO) %>%
  summarise(POB_ENT = sum(POB, na.rm = T)) %>%
  ungroup()

sequia = readxl::read_xlsx("Bases parciales/MunicipiosSequia-2.xlsx")

seq = sequia %>%
  pivot_longer(10:ncol(.)) %>%
  mutate(name = as.Date(as.numeric(name),
                        origin = "1899-12-30")) %>%
  mutate(ANIO = lubridate::year(name)) %>%
  rename(CLAVE = CVE_CONCATENADA)

# unique(datos_sequia$value)

datos_sequia = left_join(seq, pob_muni) %>%
  filter(ANIO >= 2015) %>%
  left_join(pob_ent, by = c("CVE_ENT" = "CLAVE_ENT", "ANIO")) %>%
  filter(value %in% c("D2", "D3", "D4")) %>%
  mutate(prop = 100*(POB/POB_ENT)) %>%
  ungroup() %>%
  group_by(CVE_ENT, ENTIDAD, name) %>%
  summarise(prop = sum(prop, na.rm = T)) %>%
  ungroup()

# Guardamos los datos:
openxlsx::write.xlsx(datos_sequia,
                     "Bases de datos/porcentaje_poblacion_sequia.xlsx")

