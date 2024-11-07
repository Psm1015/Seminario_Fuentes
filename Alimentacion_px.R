library(dplyr)
library(tidyverse)
library(pxR)

Alimentacion <- readLines("INPUT/DATA/Alimentacion.px", encoding = "ISO-8859-1")

Alimentacion_utf8 <- iconv(Alimentacion, from = "ISO-8859-1", to = "UTF-8")

str(Alimentacion)
Alimentacion <- readLines("INPUT/DATA/Alimentacion.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
archivo_texto_utf8 <- iconv(Alimentacion, from = "ISO-8859-1", to = "UTF-8")

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


DF <- as.data.frame(datos)
DF
view(DF)

datos_df <- DF[-(1:90), ]
datos_df
view(datos_df)

datos_df1 <- datos_df %>%
  mutate(grupo_edad = case_when(
    datos_df$Edad == "De 15 a 24 aÃ±os" ~ "De 18 a 24 aÃ±os"

  ))
datos_df1
view(datos_df1)
