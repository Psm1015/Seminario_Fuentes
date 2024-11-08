library(dplyr)
library(tidyverse)
library(pxR)


IMC <- readLines("INPUT/DATA/Indice-masa-corporal.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
IMC_utf8 <- iconv(IMC, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(IMC_utf8, archivo_utf8)

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

datos_IMC_df <- DF[-(1:20), ]
datos_IMC_df
view(datos_IMC_df)



#Datos sin ambos sexos y sin los totales de sexo edad y estudios
datos_obesidad <- datos_IMC_df %>%
  filter(
    Masa.corporal.adultos == "Obesidad (IMC>=30 kg/m2)",    # Solo obesidad
    Sexo != "Ambos sexos",                                  # Excluir ambos sexos
    Edad != "TOTAL",                                        # Excluir edad total
    Nivel.de.estudios != "TOTAL"                            # Excluir nivel de estudios total
  )
View(datos_obesidad)

#La media del porcentaje de obesidad es la siguiente
media_obesidad <- mean(datos_obesidad$value, na.rm = TRUE)
print(media_obesidad)

#La media de obesidad por hombres y mujeres es la siguiente
datos_obesidad$Sexo <- droplevels(datos_obesidad$Sexo)#Al tener en los datos iniciales la columna
#Sexo con los niveles ambos sexos, hombres y mujeres, tenemos que eliminar eses nivel.
media_por_sexo <- tapply(datos_obesidad$value, datos_obesidad$Sexo, mean, na.rm = TRUE)
print(media_por_sexo)






datos_IMC_df$Edad <- as.factor(datos_IMC_df$Edad)



