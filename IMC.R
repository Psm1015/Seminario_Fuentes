library(dplyr)
library(tidyverse)
library(pxR)

IMC <- readLines("INPUT/DATA/Indice-masa-corporal.px", encoding = "ISO-8859-1")

IMC_utf8 <- iconv(IMC, from = "ISO-8859-1", to = "UTF-8")

str(IMC)
IMC <- readLines("INPUT/DATA/Indice-masa-corporal.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
IMC_texto_utf8 <- iconv(IMC, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(IMC_texto_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos_IMC <- read.px(archivo_utf8)

#str(datos$Edad)

# Ver la estructura del archivo cargado
# str(Desempleo)
#a <-unlist(Desempleo)
#a
# Convertir a un data frame para trabajar con los datos

#Rango de edades: 18 a 24, 25 a 64, 65 o más


DF <- as.data.frame(datos_IMC)
DF
view(DF)

datos_IMC_df <- DF[-(1:90), ]
datos_IMC_df
view(datos_IMC_df)


media_por_edad <- datos_IMC_df %>%
  group_by(Edad) %>%       # Agrupar por la columna 'Edad'
  summarise(media_valor = mean(value, na.rm = TRUE))  # Calcular la media de la columna 'value'

# Mostrar los resultados
print(media_por_edad)

datos_IMC_df$Edad <- as.factor(datos_IMC_df$Edad)



