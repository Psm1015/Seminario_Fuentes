library(dplyr)
library(tidyverse)
library(pxR)

#Desempleo <- "INPUT/DATA/Desempleo.px"
#archivo <- read.px(Desempleo)

#Chatgpt (el código que está comentado es lo que había antes)
#Promt usado: estoy tratando de leer un archivo pc axis en rstudio y me sale el siguiente error. 
#Quiero que me propongas una solución para poder leer el archivo
archivo_texto <- readLines("INPUT/DATA/Desempleo.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
archivo_texto_utf8 <- iconv(archivo_texto, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(archivo_texto_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos <- read.px(archivo_utf8)

#str(datos$Edad)

# Ver la estructura del archivo cargado
# str(Desempleo)
 #a <-unlist(Desempleo)
#a
# Convertir a un data frame para trabajar con los datos

#Rango de edades: 18 a 24, 25 a 64, 65 o más


df11 <- as.data.frame(datos)
df11
view(df11)



Desempleo_full <- df11 %>%
  mutate(Edad = case_when(
    Edad %in% c('De 16 a 19 aÃ±os', 'De 20 a 24 aÃ±os') ~ '18 a 24',
    Edad %in% c('De 25 a 29 aÃ±os', 'De 30 a 34 aÃ±os', 'De 35 a 39 aÃ±os', 'De 40 a 44 aÃ±os', 'De 45 a 49 aÃ±os', 'De 50 a 54 aÃ±os', 'De 55 a 59 aÃ±os', 'De 60 a 64 aÃ±os') ~ '25 a 64',
    Edad %in% c('De 65 a 69 aÃ±os', '70 y mÃ¡s aÃ±os') ~ '65 o más',
    TRUE ~ Edad # Mantener el valor actual si no coincide con ningún rango
  ))

str(Desempleo_full)
unique(df11$Edad)
#df11$Edad <- iconv(df11$Edad, from = "ISO-8859-1", to = "UTF-8")
df11$Edad





