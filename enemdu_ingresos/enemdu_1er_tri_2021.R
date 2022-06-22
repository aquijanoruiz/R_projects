# paquetes ----------
library(foreign)
library(tidyverse)

# descargamos la base de datos desde github ----------

url <- "https://github.com/aquijanoruiz/R_projects/raw/master/enemdu_ingresos/BBDD_PUBLICACION_enero-marzo%202021_SPSS.zip"
td <- tempdir()
tf <- tempfile(tmpdir=td, fileext = ".zip")
download.file(url,tf)

enemdu_f_name <- unzip(tf, list = TRUE)$Name[2]
unzip(tf, files = enemdu_f_name, exdir=td, overwrite=TRUE)
enemdu_file_path <- file.path(td, enemdu_f_name)
enemdu <- read.spss(enemdu_file_path)

# etiqueta de las variables
enemdu_key <- data.frame(variable = names(enemdu),
                         label = attr(enemdu, 'variable.labels'))

# variables de mercado laboral ----------
# levels(enemdu$condact) condición de actividad: clasificación de la gente por situación laboral
enemdu <- as.data.frame(enemdu)
enemdu$p03 <- (as.numeric(enemdu$p03))

enemdu <- enemdu %>% 
  mutate(condactn = as.numeric(condact),
         petn = ifelse(p03 >= 15, T, F), # personas en edad de trabajar
         pean = ifelse(between(condactn, 2, 9), T, F), # población económicamente activa
         pein = ifelse(condactn == 10, T, F), # población económicamente inactiva
         empleo = ifelse(between(condactn, 2, 7), T, F), # población con empleo
         adec = ifelse(condactn == 2, T, F), # población con empleo adecuado
         sub = ifelse(between(condactn, 3, 4), T, F), # población subempleada
         sub_h = ifelse(condactn == 3, T, F), # subempleo por insuficiencia de tiempo de trabajo 
         sub_w = ifelse(condactn == 4, T, F), # subempleo por insuficiencia de ingresos 
         oinad = ifelse(condactn == 5, T, F), # otro empleo no pleno
         nr = ifelse(condactn == 6, T, F), # empleo no remunerado
         nc = ifelse(condactn == 7, T, F), # empleo no clasificado
         desempleo = ifelse(between(condactn, 8, 9), T, F), # desempleo
         desemab = ifelse(condactn == 8, T, F), # desempleo abierto
         desemoc = ifelse(condactn == 9, T, F), # desempleo oculto
         desem1 = ifelse(between(condactn, 8, 9) & p37 == 1, T, F), # desempleo cesante
         desem2 = ifelse(between(condactn, 8, 9) & p37 == 2, T, F), # desempleo nuevo
         t_a = 1) # población total

# desagregación de la secemp ----------
# levels(enemdu$secemp) sectores de los empleados (formal, informal, doméstico)

enemdu <- enemdu %>% 
  mutate(secempn = as.numeric(secemp),
         formal = ifelse(secempn == 1 & p03>=15, T, F), # Población con empleo en el sector formal
         informal = ifelse(secempn == 2 & p03>=15, T, F), # Población con empleo en el sector informal
         empdom = ifelse(secempn == 3 & p03>=15, T, F), # Población con empleo doméstico
         nocla = ifelse(secempn == 3 & p03>=15, T, F)) # Población con empleo no clasificado por sector

# grupos de edad ----------

enemdu <- enemdu %>%
  mutate(menor10 = ifelse(p03 < 10, T, F), # Población menor de 10 años
         pobla10 = ifelse(p03 >= 10, T, F), # Población de 10 años y más
         pobla15 = ifelse(p03 >= 15, T, F), # Población de 15 años y más
         pobla24 = ifelse(between(p03, 15, 24), T, F), # Población 15-24 años
         pobla34 = ifelse(between(p03, 25, 34), T, F), # Población  25-34 años
         pobla44 = ifelse(between(p03, 35, 44), T, F), # Población de 35-44 años
         pobla64 = ifelse(between(p03, 45, 64), T, F), # Población de 45-64 años
         pobla65 = ifelse(p03 >= 65, T, F), # Población de 65 años y más 
         menor15 = ifelse(p03 < 15, T, F)) # Población menor de 15 años



# empleado público y privado ----------
# levels(enemdu$p42)

enemdu <- enemdu %>%
  mutate(claempl = factor(case_when(is.na(p42) ~ NA_character_,
                                    as.numeric(p42) == 1 ~ "publico",
                                    TRUE ~ "privado"), 
                          levels = c("privado", "publico")))

# asalariado e independiente ----------

enemdu <- enemdu %>%
  mutate(asalind = factor(case_when(is.na(p42) ~ NA_character_,
                                    between(as.numeric(p42), 1,4) | as.numeric(p42) == 10 ~ "asalariado",
                                    between(as.numeric(p42), 5,6) ~ "independiente",
                                    TRUE ~ NA_character_), 
                          levels = c("asalariado", "independiente")))

# horas de trabajo ----------
# ¿Cuántas horas trabaja ( ) habitualmente a la semana en su ...?
enemdu <- enemdu %>%
  mutate(p51a = as.numeric(p51a), # ocupación principal
         p52b = as.numeric(p51b), # ocupación secundaria
         p51a = ifelse(p51a == 999, NA, p51a),
         p51b = ifelse(p51b == 999, NA, p51b)) %>%
  rowwise() %>%
  mutate(horas_total = sum(p51a, p51b, na.rm = TRUE))
         
# horas efectivas ----------
# ¿Cuántas horas trabajó la semana pasada?
enemdu <- enemdu %>%
  mutate(p24 = as.numeric(p24),
         p24 = ifelse(p51a == 999, NA, p24))
         
         
is.na(enemdu$p42)
as.numeric(enemdu$p42)
levels(enemdu$claempl)         
levels(enemdu$periodo)
levels(enemdu$p42)
summary(enemdu$p42)
class(enemdu$p51a)

view(enemdu %>% select(p51a, p51b, horas)) 

sum(1,3,5)

