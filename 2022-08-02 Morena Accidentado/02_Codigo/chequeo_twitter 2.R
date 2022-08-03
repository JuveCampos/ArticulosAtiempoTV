
# Librerias ----
library(tidyverse)
library(rtweet)
library(leaflet)

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

data_tweets <- conglomerada %>%
  do.call(rbind, .)

# Datos ----
# query = 'Morena'
# data_tweets <- rtweet::search_tweets2(q = query,
#                        n = 10000,
#                        type = "recent",
#                        include_rts = F,
#                        retryonratelimit = FALSE,
#                        verbose = TRUE)

# data_tweets <-

# max(data_tweets$created_at)
bd <- data_tweets

cols <- c("brown")
  # RColorBrewer::brewer.pal(7, "Greens")[4:7]
# scales::show_col(cols)
# scales::show_col(cols)

pal_num <- colorNumeric(palette = cols,
                        domain = c(0, 200),
                        reverse = T)

# Nube de palabras:

data = unique(bd$text)
stop_words = c("méxico")
num_words = 400
background = "white"
# mask = "apple.png"
tamanio = 0.5

# create_wordcloud <- function(data,
#                              stop_words = c(),
#                              num_words = 100,
#                              background = "white",
#                              mask = NULL,
#                              tamanio = 0.5) {
# Checar si esta instalado Pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(wordcloud2, tm, stringr)

# Pre-Función para eliminar simbolos raros
quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))

# If text is provided, convert it to a dataframe of word frequencies
# Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras
if (is.character(data)) {
  # Convertimos a Corpus
  corpus <- Corpus(VectorSource(data))
  # Convertimos el texto dentro del Corpus a Minusculas
  corpus <- tm_map(corpus, tolower)
  # Removemos la puntuacion (.,-!?)
  corpus <- tm_map(corpus, removePunctuation)
  # Removemos los numeros
  corpus <- tm_map(corpus, removeNumbers)
  # Removemos los signos de admiracion e interrogacion al reves
  corpus <- tm_map(corpus, quitar_signos)
  # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
  # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
  corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
  # Generamos una matriz para hacer el conteo
  tdm <- as.matrix(TermDocumentMatrix(corpus))
  # Obtenemos el numero de la frecuencia de cada palabra
  data <- sort(rowSums(tdm), decreasing = TRUE)
  # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra
  data <- data.frame(word = names(data), freq = as.numeric(data))
}

freq_palabras <<- data
# freq_palabras[1,2] <- 1300

# Make sure a proper num_words is provided
# Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
if (!is.numeric(num_words) || num_words < 3) {
  num_words <- 3
}

pal_num <- colorNumeric(palette = cols,
                        domain = freq_palabras$freq,
                        reverse = T)

color_wc <- pal_num(freq_palabras$freq)

# Grab the top n most common words
# Recortamos la base de datos de palabras a un numero `n` especificado
data <- head(data, n = num_words)
if (nrow(data) == 0) {
  return(NULL)
}

data

data$freq = sqrt(data$freq)

# data$word[1] <- "#YoDefiendoAlCIDE"
# data$word[2] <- "#SomosCIDE"

# color = "random-dark"
figPath = system.file("examples/t.png",package = "wordcloud2")
class(figPath)
wordcloud2(data,
           backgroundColor = background,
           color = color_wc,
           # figPath = figPath,
           # shape = "square",
           fontFamily = "Poppins",
           size = 0.5)
