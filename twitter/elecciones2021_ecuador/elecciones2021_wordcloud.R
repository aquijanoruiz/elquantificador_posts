# paquetes

library(rtweet)
library(tidyverse)
library(stringr)
library(tm)
library(syuzhet)
library(wordcloud)
library(qdap)
library(hrbrthemes)
library(RColorBrewer)

# tw_trends <- get_trends("Ecuador") # El hashtag con mas tendencia es #Elecciones2021Ec
# elecciones2021 <- search_tweets("#Elecciones2021Ec", n = 18000, include_rts = FALSE)

elecciones2021_url <- 
  "https://github.com/aquijanoruiz/elquantificador_posts/raw/master/twitter/elecciones2021_ecuador/elecciones2021.rds"

elecciones2021 <- readRDS(url(elecciones2021_url))

# Una función para eliminar el ruido en el texto  ----------

clean_text <- function(text) {
  new_text <- tolower(text)
  new_text <- str_replace_all(new_text,"http\\S*", "") # Elimina urls
  new_text <- str_replace_all(new_text,"@\\S*", "") # Elimina los @
  new_text <- str_replace_all(new_text,"[[:punct:]]", " ") # Elimina los signos de puntuación
  new_text <- str_replace_all(new_text,"[[:digit:]]", " ") # Elimina los números
  new_text <- str_replace_all(new_text,"\\s[a-z]{1}\\s", " ") # Elimina las palabras de un solo caracter
  new_text <- str_replace_all(new_text,"\\s[a-z]{1}\\s", " ") # Elimina las palabras de un solo caracter (otra vez)
  new_text <- str_replace_all(new_text,"[\\s]+", " ") # Elimina los espacios
  return(new_text)
}

## --------------------------------------------------------------------------- ##
## ----------------------- Hacemos una nube de palabras ---------------------- ##
## --------------------------------------------------------------------------- ##

texto <- elecciones2021$text
texto_limpio <- clean_text(elecciones2021)

texto_corpus <-  texto_limpio %>% 
  VectorSource() %>% 
  Corpus()

# Eliminamos palabras innecesarias
texto_corpus <- tm_map(texto_corpus, removeWords, stopwords("spanish"))
freq_terms(texto_corpus, 70) # Vemos las 70 palabras con mayor frecuencia

# Eliminamos más palabras innecesarias
custom_stopwords <- c("na", "false", "for", "photo", "android", "ea", "iphone", "web", "app", 
                      "true", "com", "und", "si", "ser")

texto_corpus <- tm_map(texto_corpus, removeWords, custom_stopwords)

# Hacemos una nube de palabras
wordcloud(texto_corpus, colors = brewer.pal(6, "Dark2"), max.words = 60,
          scale = c(3.8,0.7), random.order = FALSE)
