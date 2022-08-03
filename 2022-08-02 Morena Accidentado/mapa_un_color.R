# Mapa
library(sf)
library(tidyverse)

# Datos
shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

tibble::tribble(
  ~cve_ent,                  ~ent, ~is,
      "01",      "Aguascalientes", 1,
      "02",     "Baja California", 0,
      "03", "Baja California Sur", 0,
      "04",            "Campeche", 0,
      "05",            "Coahuila", 0,
      "06",              "Colima", 0,
      "07",             "Chiapas", 1,
      "08",           "Chihuahua", 0,
      "09",    "Ciudad de México", 1,
      "10",             "Durango", 1,
      "11",          "Guanajuato", 1,
      "12",            "Guerrero", 1,
      "13",             "Hidalgo", 0,
      "14",             "Jalisco", 1,
      "15",              "México", 0,
      "16",           "Michoacán", 1,
      "17",             "Morelos", 1,
      "18",             "Nayarit", 0,
      "19",          "Nuevo León", 0,
      "20",              "Oaxaca", 1
      "21",              "Puebla", 1,
      "22",           "Querétaro", 0,
      "23",        "Quintana Roo", 0,
      "24",     "San Luis Potosí", 0,
      "25",             "Sinaloa", 0,
      "26",              "Sonora", 0,
      "27",             "Tabasco", 1,
      "28",          "Tamaulipas", 0,
      "29",            "Tlaxcala", 0,
      "30",            "Veracruz", 1,
      "31",             "Yucatán", 0,
      "32",           "Zacatecas" 0
  )

