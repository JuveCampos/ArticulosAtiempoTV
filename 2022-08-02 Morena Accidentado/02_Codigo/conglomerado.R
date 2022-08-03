# librerias
library(tidyverse)

files <- str_c("01_Datos/", list.files("01_Datos/"))

conglomerada <- lapply(files, function(x){
  # x = files[1]
  edo <- str_remove_all(x, pattern = "01_Datos/|\\.xlsx")
  data <- readxl::read_xlsx(x) %>%
    mutate(estado = edo) %>%
    filter(retweet_count > 20 | favorite_count > 40)
  print(edo)
  return(data)
})

conglomerada[[20]] <- NULL
conglomerada[[18]] <- NULL

lapply(conglomerada, ncol)

conglomerada %>%
  do.call(rbind, .) %>%
  openxlsx::write.xlsx("conglomerada.xlsx")

# %>%
#   do.call(rbind,.)

# openxlsx::write.xlsx(conglomerada, "conglomerada.xlsx")

