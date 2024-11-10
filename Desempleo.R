library(dplyr)
library(tidyverse)
library(pxR)


#Chatgpt (el código que está comentado es lo que había antes)
#Promt usado: estoy tratando de leer un archivo pc axis en rstudio y me sale el siguiente error. 
#Quiero que me propongas una solución para poder leer el archivo
archivo_texto <- readLines("INPUT/DATA/Desempleo.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
archivo_texto_utf8 <- iconv(archivo_texto, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(archivo_texto_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos <- read.px(archivo_utf8)


df11 <- as.data.frame(datos)
df11
view(df11)


#Rango de edades: 18 a 24, 25 a 64, 65 o más
#Agrpuamineto de las edades por los rangos que hemos deifinifo
Desempleo_full <- df11 %>%
  mutate(Grupo_edad = case_when(
    Edad == "De 16 a 19 aÃ±os" ~ "18 a 24 aÃ±os",
    Edad == "De 20 a 24 aÃ±os" ~ "18 a 24 aÃ±os",
    Edad == "De 25 a 29 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 30 a 34 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 35 a 39 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 40 a 44 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 45 a 49 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 50 a 54 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 55 a 59 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 60 a 64 aÃ±os" ~ "25 a 64 aÃ±os",
    Edad == "De 65 a 69 aÃ±os" ~ "65 o mÃ¡s",
    Edad =="70 y mÃ¡s aÃ±os" ~ '65 o mÃ¡s',
  ))


#Eliminar filas es la que salia el valor NA
Desempleo_full1 <- Desempleo_full[-c((1:144), (1873:2016), (3745:3888)), ]
Desempleo_full1
view(Desempleo_full1)

Desempleo_df <- Desempleo_full1 %>% select(-Edad)
view(Desempleo_df)


#Para eliminar los valores de ambos sexos y total
datos_Desempleo <-  Desempleo_df %>%
  filter(
    Sexo != "Ambos sexos",                                  
    Tiempo.de.bÃºsqueda.de.empleo != "Total",
    Periodo == "2021"                             
  )
view(datos_Desempleo)


#Gráficas
library(ggplot2)

# Gráfica de desempleo por grupo de edad en 2021
ggplot(datos_Desempleo, aes(x = Grupo_edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Desempleo por Grupo de Edad en 2021", x = "Grupo de Edad", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Gráfica para observar la tendencia de desempleo en distintos periodos
ggplot(datos_Desempleo, aes(x = Periodo, y = value, fill = Grupo_edad)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tendencia del Desempleo por Grupo de Edad", x = "Año", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("18 a 24 aÃ±os" = "steelblue", "25 a 64 aÃ±os" = "salmon", "65 o mÃ¡s" = "#CDDC39")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Gráfica para comparar el desempleo entre hombres y mujeres en 2001 confiltro Grupo_edad
ggplot(datos_Desempleo %>% filter(Grupo_edad == "18 a 24 aÃ±os"), aes(x = Sexo, y = value, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Desempleo entre Hombres y Mujeres (18 a 24 años, 2001)", x = "Sexo", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







