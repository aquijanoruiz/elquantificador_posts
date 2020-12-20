
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

ninenineTOna <- function(x){ 
  y = ifelse(x == 77 | x == 88 | x == 99 , NA, x)
  return(y)
}

###############################################################
####                   Limpieza de datos                   ####
###############################################################

# base de datos de mujeres
m_data_plot <- mujeres %>% 
  mutate(sexo_jovenes = f2_s8_803,
         sexo_mayores = factor(case_when(
           f2_s8_831 == 88 | f2_s8_831 == 77 ~ NA_character_,
           f2_s8_831 == 99 ~ "no", TRUE ~ "si"), levels = c("si", "no")),
         relaciones_sexuales = coalesce(sexo_jovenes, sexo_mayores),
         edad_1er_sexo = ninenineTOna(coalesce(f2_s8_804, f2_s8_831)),
         anio_nacimiento = f2_s1_100_3,
         sexo = "mujer",
         peso = fexp) %>%
  select(sexo, anio_nacimiento, relaciones_sexuales,  edad_1er_sexo, peso)

# base de datos de hombres
h_data_plot <- hombres %>% 
  mutate(relaciones_sexuales = factor(case_when(
          f3_s2_202 == "no desea contestar" ~ NA_character_,
          f3_s2_202 == "no" ~ "no", TRUE ~ "si")),
         edad_1er_sexo = ninenineTOna(f3_s2_203),
         anio_nacimiento = f3_s1_1_5,
         sexo = "hombre",
         peso = fexp) %>% 
  select(sexo, anio_nacimiento, relaciones_sexuales,  edad_1er_sexo, peso)

# unimos las bases de datos
data_plot <- rbind(m_data_plot, h_data_plot)

# iniciación sexual temprana
data_plot$sexo_precoz <- factor(with(data_plot, case_when(
  is.na(relaciones_sexuales) ~ NA_character_, relaciones_sexuales == "no" ~ "no", 
  edad_1er_sexo > 16 ~ "no", TRUE ~ "si")), levels = c("si", "no"))

###############################################################
####                        Graficos                       ####
###############################################################

# grafico edad promedio de iniciación sexual por sexo
data_plot %>% filter(between(anio_nacimiento, 1979,1999)) %>% 
  select(sexo, anio_nacimiento, edad_1er_sexo, peso) %>% group_by(sexo, anio_nacimiento) %>%
  summarize(prom_edad_1er_sexo = mean(edad_1er_sexo, na.rm = TRUE, w = peso)) %>%
  ggplot(aes(x = anio_nacimiento, y = prom_edad_1er_sexo, color = sexo)) + geom_line() +
  scale_y_continuous(minor_breaks = NULL) + 
  labs(x = "año de nacimiento", y = "", 
       title = "Edad promedio de iniciación sexual en el Ecuador") +
  theme_minimal() + theme(plot.title = element_text(color = "black", size = 14, face = "bold.italic"))

# grafico sobre porcentaje de iniciación sexual temprana por sexo
data_plot %>% filter(between(anio_nacimiento, 1979,1999)) %>% 
  select(sexo, anio_nacimiento, sexo_precoz, peso) %>% group_by(sexo, anio_nacimiento) %>%
  summarize(prev_sexo_precoz = mean(sexo_precoz == "si", na.rm = TRUE)) %>%
  ggplot(aes(x = anio_nacimiento, y = prev_sexo_precoz, color = sexo)) + geom_line() +
  scale_y_continuous(labels = percent_format(accuracy = 5L)) +
  labs(x = "año de nacimiento", y = "", title = "Porcentaje de inciación sexual temprana en el Ecuador", 
       subtitle = "Porcentaje de hombres y mujeres que tuvieron su primera relación a los 16 años o antes") +
  theme_minimal() + theme(plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
                          plot.subtitle = element_text(size = 10, face = "italic"))
