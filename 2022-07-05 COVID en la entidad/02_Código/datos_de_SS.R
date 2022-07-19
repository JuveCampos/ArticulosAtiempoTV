library(tidyverse)

datos <- read_csv("01_Datos/220705COVID19MEXICO.csv")
object.size(datos)
names(datos)

head(datos$FECHA_DEF)

datos_2 <- datos %>%
  filter(!is.na(FECHA_DEF))

datos_2

# Guardar RDS ----
saveRDS(datos_2,
        "01_Datos/datos_ssalud_fallecimientos.rds")
