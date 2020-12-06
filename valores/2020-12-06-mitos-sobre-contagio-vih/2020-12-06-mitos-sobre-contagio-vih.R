
###############################################################
####                   Descargar los datos                 ####
###############################################################

if(!require(readstata13)) install.packages("readstata13", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")

# Base de datos Ensanut ----------
# Le damos un nombre al url
url <- "https://github.com/aquijanoruiz/elquantificador_posts/raw/master/databases/salud/BDD_ENSANUT_2018_STATA_.zip"
# Creamos un directorio temporal
td <- tempdir()
# Creamos una carpeta temporal
tf <- tempfile(tmpdir=td, fileext = ".zip")
# Descargamos los discap en la carpeta temporal
download.file(url,tf)

# Obtenemos el nombre del archivo dentro del archivo zip, lo descomprimimos (unzip), obtenemos el nombre del 
# parche, y finalmente lo cargamos
# base de datos de mujeres
mujeres.f.name <- unzip(tf, list=TRUE)$Name[5] 
unzip(tf, files=mujeres.f.name, exdir=td, overwrite=TRUE)
mujeres.f.path <- file.path(td, mujeres.f.name)
mujeres <- read.dta13(mujeres.f.path)

data.key.mujeres <- data.frame(variable = names(mujeres), 
                                label = attr(mujeres,"var.labels"))

# base de datos hombres
hombres.f.name <- unzip(tf, list=TRUE)$Name[8] 
unzip(tf, files=hombres.f.name, exdir=td, overwrite=TRUE)
hombres.f.path <- file.path(td, hombres.f.name)
hombres <- read.dta13(hombres.f.path)

data.key.hombres <- data.frame(variable = names(hombres), 
                               label = attr(hombres,"var.labels"))

# seleccionamos las preguntas sobre sida
mujeres <- mujeres %>% select(edadanios, f2_s10_1011_1, f2_s10_1011_2, f2_s10_1011_3, 
                              f2_s10_1011_4, f2_s10_1011_5, fexp)

hombres <- hombres %>% select(edadanios, f3_s4_411_1, f3_s4_411_2, f3_s4_411_3, 
                              f3_s4_411_4, f3_s4_411_5, fexp) 

###############################################################
####                        Analisis                      ####
###############################################################

# Cambiamos los nombres a las columnas
names(mujeres) <- c("edadanios", "mano", "beso", "mosquito", "cuchara", "trabajar", "fexp")
names(hombres) <- c("edadanios", "mano", "beso", "mosquito", "cuchara", "trabajar", "fexp")

# Unimos las dos bases de datos
info_sida <- rbind(mujeres, hombres)

# Transformamos los valores a expresiones boolean y eliminamos los NAs
info_sida[,2:6] <- sapply(info_sida[,2:6], function(x){
  binary <- case_when(is.na(x) ~ NA,
                      x == "si" ~ TRUE,
                      x == "no" ~ FALSE,
                      TRUE ~ NA)
  return(binary)
})

info_sida <- na.omit(info_sida)

# Calculamos la media ponderada considerando el factor de expansion
resultados <- sapply(info_sida[,2:6], weighted.mean, w = info_sida$fexp)
resultados <- data.frame(pregunta = names(resultados), freq = resultados)

# Hacemos los gr??ficos
preg_etiqueta <- c("Darle la mano a alguien \n que tiene VIH/SIDA?", 
                   "Besar en la frente a alguien \n que tiene VIH/SIDA?",
                   "Ser picado por un mosquito o zancudo?", 
                   "Usar cucharas, tenedores, platos o vasos \n usados por alguien que tiene VIH/SIDA?",
                   "Trabajar con una persona \n que tiene VIH/SIDA?")

resultados$pregunta <- preg_etiqueta

resultados %>% ggplot(aes(x = reorder(pregunta, freq), y = freq)) + 
  geom_bar(stat = "identity", fill = "skyblue" ) +
  coord_flip() + scale_y_continuous(labels = percent, limits = c(0, 0.6)) + 
  labs(title = "??Piensa usted que una persona puede \n infectarse del VIH/SIDA al ...:", x = "", y = "") +
  geom_text(aes(label= scales::percent(freq, accuracy = 0.1)), hjust=-0.2, size=3, fontface = "italic") + 
  theme_ipsum() + theme(legend.position = "none") + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
        axis.text.y = element_text(size=10), axis.text.x = element_text(size=10)) 