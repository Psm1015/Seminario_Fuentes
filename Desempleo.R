library(dplyr)
library(tidyverse)
library(pxR)


#Lectura del archivo línea por línea
archivo_texto <- readLines("INPUT/DATA/Desempleo.px", encoding = "ISO-8859-1")

#Modifica la codificación original del texto a la codificación UTF-8
archivo_texto_utf8 <- iconv(archivo_texto, from = "ISO-8859-1", to = "UTF-8")

#Creación del archivo temporal .px
archivo_utf8 <- tempfile(fileext = ".px")

#Se escribe el contenido codificado en UTF-8 al archivo temporal
writeLines(archivo_texto_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos <- read.px(archivo_utf8)


Desempleo_frame <- as.data.frame(datos)
Desempleo_frame
#view(Desempleo_frame)


#Agrupación de los rangos de edades y correción de caraceteres extraños
Desempleo_agrup <- Desempleo_frame %>%
  transmute(
    Edad = case_when(
      Edad == "De 16 a 19 aÃ±os" ~ "De 18 a 24 años",
      Edad == "De 20 a 24 aÃ±os" ~ "De 18 a 24 años",
      Edad == "De 25 a 29 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 30 a 34 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 35 a 39 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 40 a 44 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 45 a 49 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 50 a 54 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 55 a 59 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 60 a 64 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 65 a 69 aÃ±os" ~ "65 o más",
      Edad == "70 y mÃ¡s aÃ±os" ~ "65 o más",
    ),
    Tiempo.de.bÃºsqueda.de.empleo = case_when(
      Tiempo.de.bÃºsqueda.de.empleo == "2 aÃ±os o mÃ¡s" ~ "2 años o más",
      Tiempo.de.bÃºsqueda.de.empleo == "Ya ha encontrado empleo" ~ "Ya ha encontrado empleo",
      Tiempo.de.bÃºsqueda.de.empleo == "De 1 mes a menos de 3 meses" ~ "De 1 mes a menos de 3 meses",
      Tiempo.de.bÃºsqueda.de.empleo == "De 3 meses a menos de 6 meses" ~ "De 3 meses a menos de 6 meses",
      Tiempo.de.bÃºsqueda.de.empleo == "De 6 meses a menos de 1 aÃ±o" ~ "De 6 meses a menos de 1 año",
      Tiempo.de.bÃºsqueda.de.empleo == "De 1 aÃ±o a menos de 2 aÃ±os" ~ "De 1 año a menos de 2 años",
      Tiempo.de.bÃºsqueda.de.empleo == "Total" ~ "Total",
      
    ),
    Periodo = Periodo,
    Tiempo.de.bÃºsqueda.de.empleo=Tiempo.de.bÃºsqueda.de.empleo,
    Edad = Edad,
    Sexo = Sexo,
    value = value, 
  )%>%
  #Cambio del nombre de la columna tiempo de búsqueda de empleo
  mutate(Desempleo_agrup,  Tiempo_de_búsqueda_de_empleo= Tiempo.de.bÃºsqueda.de.empleo)%>%
  select(-Tiempo.de.bÃºsqueda.de.empleo)
#view(Desempleo_agrup)


#Eliminar filas es la que salia el valor NA
Desempleo_no_NA <- Desempleo_agrup%>%
  drop_na(value, Tiempo_de_búsqueda_de_empleo, Edad, Periodo, Sexo)
Desempleo_no_NA
#view(Desempleo_no_NA)



#Elimino los valores de ambos sexos y total, y selecciono solo los datos del 2021
Desempleo_data <-  Desempleo_no_NA %>%
  filter(
    Sexo != "Ambos sexos",                                  
    Tiempo_de_búsqueda_de_empleo != "Total",
    Periodo == "2021"                             
  )
#view(Desempleo_data)


#GRÁFICAS
library(ggplot2)

#GRÁFICA DE DESEMPLEO POR GRUPO DE EDAD EN 2021
ggplot(Desempleo_data, aes(x = Edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill=factor(Sexo))) +
  labs(title = "Desempleo por Grupo de Edad en 2021", x = "Grupo de Edad", y = "Cantidad de Desempleo") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#GRÁFICA PARA OBSERVAR LA TENDENCIA DE DESEMPLEO EN DSTINTOS PERIODOS
ggplot(Desempleo_data, aes(x = Periodo, y = value, fill = Edad)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill=factor(Sexo))) +
  labs(title = "Tendencia del Desempleo por Edad", x = "Año", y = "Cantidad de Desempleo") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#GRÁFICA PARA COMPARAR EL DESEMPLEO ENTRE HOMBRES Y MUJERES EN 2021 POR GRUPO_EDAD
Desempleo_HM <- ggplot(Desempleo_data %>% filter(Edad == "De 18 a 24 años"), aes(x = Sexo, y = value, fill = Sexo)) +
  geom_bar(stat = "identity",  aes(fill=factor(Sexo))) +
  labs(title = "Comparación de Desempleo entre Hombres y Mujeres (18 a 24 años, 2021)", x = "Sexo", y = "Cantidad de Desempleo") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
Desempleo_HM

ggsave(
  filename = "Desempleo_Hombres_Mujeres.jpeg",
  plot = Desempleo_HM ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Desempleo", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


#RELACIÓN TIMEPO BÚSQUEDA DE EMPLEO CON IMC
Desempleo_IMC <- inner_join( 
  Desempleo_data, 
  datos_IMC_df, 
  by = join_by(Edad, Sexo), 
)
#view(Desempleo_IMC)


#Nos quedamos solo con la obesidad y eliminamos los valores total
Desempleo_IMC_filtrado<- Desempleo_IMC%>%
  drop_na()%>%
  filter(
    Masa.corporal.adultos=="Obesidad (IMC>=30 kg/m2)",
    Masa.corporal.adultos!="TOTAL",
    Nivel.de.estudios!="TOTAL"
    
  )

#view(Desempleo_IMC_filtrado)

#GRÁFICO QUE MUESTRA EL IMC DE HOMBRES Y MUJERES SEGÚN EL TIEMPO DE DESEMPLEO
Desempleo_obesidad <- ggplot(data = Desmepleo_IMC_filtrado, aes(x = Tiempo_de_búsqueda_de_empleo, y = value, fill=Sexo)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill=factor(Sexo))) +
  facet_wrap(~ Masa.corporal.adultos) +  # Dividir por grupos de edad
  labs(
    title = "Índice de masa corporal según el tiempo de desempleo",
    x = "Tiempo de desempleo", #creo que está mal
    y = "Personas en desempleo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para que se vean bien
  )
Desempleo_obesidad

ggsave(
  filename = "Desempleo_obesidad.jpeg",
  plot = Desempleo_obesidad ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Desempleo", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


#RELACIÓN TIEMPO DE BÚSQUEDA DE EMPLEO CON ALIMENTOS
Desempleo_Alimentacion <- inner_join(
  Desempleo_data, 
  datos_Alimentacion, 
  by = join_by(Edad,Sexo),    
)

#view(Desempleo_Alimentacion)

# Crear gráfico de barras que muestra la relación entre el tiempo de búsqueda de empleo, alimentos, frecuencia y sexo
ggplot(Desempleo_Alimentacion, aes(x = `Frecuencia`, y = Tiempo_de_búsqueda_de_empleo, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +  
  labs(
    title = "Relación entre el tiempo de búsqueda de empleo, frecuencia de consumo de alimentos y sexo",
    x = "Tiempo de búsqueda de empleo",
    y = "Frecuencia de consumo de alimentos",
    fill = "Sexo"
  ) 









