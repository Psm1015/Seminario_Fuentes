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
df11 <- as.data.frame(datos)
df11
view(df1)
