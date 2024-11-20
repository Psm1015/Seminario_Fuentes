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

# Filter para quitar diversos rangos de edad
DF1 <- DF %>%
  filter(!(Edad %in% c("De 25 a 34 aÃ±os", "De 45 a 54 aÃ±os", "De 55 a 64 aÃ±os", "De 75 y mÃ¡s aÃ±os")))
DF1
view(DF1)

# Crear otra columna (Grupo_Edad) cambiando los rangos de Edad
#datos_df <- DF1 %>%
#mutate(Grupo_edad = case_when(
#    Edad == "De 15 a 24 aÃ±os" ~ "De 18 a 24 aÃ±os",
#    Edad == "De 25 a 34 aÃ±os" ~ "De 25 a 64 aÃ±os",
#    Edad == "De 35 a 44 aÃ±os" ~ "De 25 a 64 aÃ±os",
#    Edad == "De 45 a 54 aÃ±os" ~ "De 25 a 64 aÃ±os",
#   Edad == "De 55 a 64 aÃ±os" ~ "De 25 a 64 aÃ±os",
#    Edad == "De 65 a 74 aÃ±os" ~ "De 65 y mÃ¡s aÃ±os",
#    Edad == "De 75 y mÃ¡s aÃ±os" ~ "De 65 y mÃ¡s aÃ±os"
#  ))
#datos_df
#view(datos_df)
datos_df1 <- DF1 %>%
  transmute(
    Edad = case_when(
      Edad == "De 15 a 24 aÃ±os" ~ "De 18 a 24 aÃ±os",
      #    Edad == "De 25 a 34 aÃ±os" ~ "De 25 a 64 aÃ±os",
      Edad == "De 35 a 44 aÃ±os" ~ "De 25 a 64 aÃ±os",
      #    Edad == "De 45 a 54 aÃ±os" ~ "De 25 a 64 aÃ±os",
      #   Edad == "De 55 a 64 aÃ±os" ~ "De 25 a 64 aÃ±os",
      Edad == "De 65 a 74 aÃ±os" ~ "De 65 y mÃ¡s aÃ±os",
      #    Edad == "De 75 y mÃ¡s aÃ±os" ~ "De 65 y mÃ¡s aÃ±os"
    ),
    Frecuencia = Frecuencia,
    Alimentos = Alimentos,
    Sexo = Sexo,
    value = value
  )
view(datos_df1)

# ELIMINAR TODAS LAS FILAS QUE TENGAN NA EN LA COLUMNA Grupo_edad
df_sin_NA <- datos_df1 %>% 
  filter(!is.na(Edad))
df_sin_NA
view(df_sin_NA)

#ELIMINAR LA COLUMNA EDAD
Alimentacion_df <- df_sin_NA %>% 
  select(-Edad)
view(Alimentacion_df)

datos_Alimentacion <-  df_sin_NA%>%
  filter(
    Frecuencia != "TOTAL",                                  # Excluir frecuencia total
  )
view(datos_Alimentacion)

#datos_Alimentacion <- datos_Alimentacion_duplicaciones %>%
#  group_by(Frecuencia, Alimentos, Sexo, Grupo_edad) %>%
#  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop')
#view(datos_Alimentacion)

# GRÁFICA QUE RELACIONA QUE COME CADA RANGO DE EDAD A DIARIO.
library(ggplot2)
Alimentacion_ADiario <- datos_Alimentacion %>%
  filter(
    Frecuencia == "A diario",
    (!(Alimentos %in% c("Productos lÃ¡cteos", "Aperitivos o comidas saladas de picar", "Zumo natural de frutas o verduras")))
 
  )
view(Alimentacion_ADiario)

ggplot(Alimentacion_ADiario, aes(x = Edad, y = value, fill = Alimentos)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Sexo) +
  labs(title = "Porcentaje de Consumo Diario de Alimentos por Rango de Edad y Sexo",
       x = "Rango de Edad",
       y = "Porcentaje de Consumo Diario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME PESCADO 
# 3 O MÁS VECES A LA SEMANA PERO NO A DIARIO
Alimentacion_pescado <- datos_Alimentacion %>%
  filter(
    Frecuencia == "3 o mÃ¡s veces a la semana pero no a diario",
    Alimentos == "Pescado"
  )
view (Alimentacion_pescado)

ggplot(Alimentacion_pescado, aes(x = Edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Porcentaje de Consumo de Pescado 3 o más Veces a la Semana, No Diario por Sexo y Rango de Edad",
    x = "Rango de Edad",
    y = "Porcentaje de Personas"
  ) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon", "Ambos sexos" = "yellow")) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME CARNE 
# 3 O MÁS VECES A LA SEMANA PERO NO A DIARIO
Alimentacion_carne <- datos_Alimentacion %>%
  filter(
    Frecuencia == "3 o mÃ¡s veces a la semana pero no a diario",
    Alimentos == "Carne"
  )
view(Alimentacion_carne)
ggplot(Alimentacion_carne, aes(x = Edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Porcentaje de Consumo de Carne 3 o más Veces a la Semana, No Diario por Sexo y Rango de Edad",
    x = "Rango de Edad",
    y = "Porcentaje de Personas"
  ) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon", "Ambos sexos" = "yellow")) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME COMIDA RAPIDA  
# 1 O 2 VECES A LA SEMANA
Alimentacion_rapida <- datos_Alimentacion %>%
  filter(
    Frecuencia == "1 o 2 veces a la semana",
    Alimentos == "Comida rÃ¡pida"
  )
view(Alimentacion_rapida)
ggplot(Alimentacion_rapida, aes(x = Edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Porcentaje de Consumo de comida rapida 1 o 2 veces a la Semana",
    x = "Rango de Edad",
    y = "Porcentaje de Personas"
  ) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon", "Ambos sexos" = "yellow")) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Alimentacion_IMC <- inner_join(
  Alimentacion_rapida,  # Primer DataFrame
  df_tabla_media,
  by = join_by(Edad, Sexo)# Segundo DataFrame  ,  # Tipo de join: "inner"  # Para diferenciar las columnas con el mismo nombre
)
view(df_combined)

