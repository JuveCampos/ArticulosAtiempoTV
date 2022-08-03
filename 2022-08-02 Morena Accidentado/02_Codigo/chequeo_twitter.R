
# Librerias ----
library(tidyverse)
library(rtweet)
library(leaflet)
library(openxlsx)

# Datos ----
edos <- tibble::tribble(
  ~cve_ent,                  ~ent,
  "00",            "Nacional",
  "01",      "Aguascalientes",
  "02",     "Baja California",
  "03", "Baja California Sur",
  "04",            "Campeche",
  "05",            "Coahuila",
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
  "16",           "Michoacán",
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
  "30",            "Veracruz",
  "31",             "Yucatán",
  "32",           "Zacatecas")


lapply(edos$ent[30:33], function(e){

  # e = edos$ent[4]

  query = str_c('morena ', e)
  data_tweets <- rtweet::search_tweets2(q = query,
                                        n = 1000,
                                        type = "recent",
                                        include_rts = F,
                                        retryonratelimit = TRUE,
                                        verbose = TRUE)

  data_tweets %>%
    arrange(-retweet_count, -favorite_count) %>%
    select(screen_name, created_at,text, retweet_count, favorite_count) %>%
    write.xlsx(file = str_c("01_Datos/", e, ".xlsx"))

  print(e)

})
