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


datos_df <- DF %>%
  mutate(Grupo_edad = case_when(
    Edad == "De 15 a 24 aÃ±os" ~ "De 18 a 24 aÃ±os",
    Edad == "De 25 a 34 aÃ±os" ~ "De 25 a 64 aÃ±os",
    Edad == "De 35 a 44 aÃ±os" ~ "De 25 a 64 aÃ±os",
    Edad == "De 35 a 44 aÃ±os" ~ "De 25 a 64 aÃ±os",
    Edad == "De 45 a 54 aÃ±os" ~ "De 25 a 64 aÃ±os",
    Edad == "De 55 a 64 aÃ±os" ~ "De 25 a 64 aÃ±os",
    Edad == "De 65 a 74 aÃ±os" ~ "De 65 y mÃ¡s aÃ±os",
    Edad == "De 75 y mÃ¡s aÃ±os" ~ "De 65 y mÃ¡s aÃ±os"
  ))
datos_df
view(datos_df)

df_sin_NA <- datos_df %>% filter(!is.na(Grupo_edad))
df_sin_NA
view(df_sin_NA)

Alimentacion_df <- df_sin_NA %>% select(-Edad)
view(Alimentacion_df)

datos_Alimentacion_duplicaciones <-  Alimentacion_df %>%
  filter(
    Sexo != "Ambos sexos",                                  # Excluir ambos sexos
    Frecuencia != "TOTAL",                                  # Excluir edad total
  )
view(datos_Alimentacion_duplicaciones)

datos_Alimentacion <- datos_Alimentacion_duplicaciones %>%
  group_by(Frecuencia, Alimentos, Sexo, Grupo_edad) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
view(datos_Alimentacion)
