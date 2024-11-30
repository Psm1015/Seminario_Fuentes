library(dplyr)
library(tidyverse)
library(pxR)



archivo_texto <- readLines("INPUT/DATA/Desempleo.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
archivo_texto_utf8 <- iconv(archivo_texto, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(archivo_texto_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos <- read.px(archivo_utf8)


Desempleo_frame <- as.data.frame(datos)
Desempleo_frame
#view(Desempleo_frame)


#Rango de edades: 18 a 24, 25 a 64, 65 o más
#Agrpuamineto de las edades por los rangos que hemos deifinifo
#Desempleo_full <- df11 %>%
 # mutate(Grupo_edad = case_when(
  #  Edad == "De 16 a 19 aÃ±os" ~ "18 a 24 aÃ±os",
   # Edad == "De 20 a 24 aÃ±os" ~ "18 a 24 aÃ±os",
    #Edad == "De 25 a 29 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 30 a 34 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 35 a 39 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 40 a 44 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 45 a 49 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 50 a 54 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 55 a 59 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 60 a 64 aÃ±os" ~ "25 a 64 aÃ±os",
    #Edad == "De 65 a 69 aÃ±os" ~ "65 o mÃ¡s",
    #Edad =="70 y mÃ¡s aÃ±os" ~ '65 o mÃ¡s',
 # ))
#view(Desempleo_full)
#colnames(Desempleo_full)

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
  mutate(Desempleo_agrup,  Tiempo_de_búsqueda_de_empleo= Tiempo.de.bÃºsqueda.de.empleo)%>%
  select(-Tiempo.de.bÃºsqueda.de.empleo)
#view(Desempleo_agrup)


#Eliminar filas es la que salia el valor NA
Desempleo_no_NA <- Desempleo_agrup[-c((1:144), (1873:2016), (3745:3888)), ]
Desempleo_no_NA
#view(Desempleo_no_NA)

#Desempleo_df <- Desempleo_full11 %>% select(-Edad)
#view(Desempleo_df)


#Para eliminar los valores de ambos sexos y total
Desempleo_data <-  Desempleo_no_NA %>%
  filter(
    Sexo != "Ambos sexos",                                  
    Tiempo_de_búsqueda_de_empleo != "Total",
    Periodo == "2021"                             
  )
#view(Desempleo_data)


#Gráficas
library(ggplot2)

# Gráfica de desempleo por grupo de edad en 2021
ggplot(Desempleo_data, aes(x = Edad, y = value, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Desempleo por Grupo de Edad en 2021", x = "Grupo de Edad", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Gráfica para observar la tendencia de desempleo en distintos periodos
ggplot(Desempleo_data, aes(x = Periodo, y = value, fill = Edad)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tendencia del Desempleo por Edad", x = "Año", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("De 18 a 24 años" = "steelblue", "De 25 a 64 años" = "salmon", "65 o más" = "yellow")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#Gráfica para comparar el desempleo entre hombres y mujeres en 2001 confiltro Grupo_edad
ggplot(Desempleo_data %>% filter(Edad == "De 18 a 24 años"), aes(x = Sexo, y = value, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Desempleo entre Hombres y Mujeres (18 a 24 años, 2001)", x = "Sexo", y = "Cantidad de Desempleo") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


Desempleo_IMC <- inner_join(
  Desempleo_data, 
  datos_IMC_df, 
  by = join_by(Edad, Sexo),
  relationship = "many-to-many" #generará un nuevo data frame con todas las combinaciones de 
                                #filas coincidentes entre los dos marcos de datos
)
#view(Desempleo_IMC)

Desempleo_Alimentacion <- inner_join(
  Desempleo_data, 
  datos_Alimentacion, 
  by = join_by(Edad,Sexo),    #relación tiempo de búsqueda de empleo con alimentos
  relationship = "many-to-many"
)%>%
  select(-Edad, -Periodo, -Sexo, -value.x, -Frecuencia, -value.y)

#view(Desempleo_Alimentacion)









