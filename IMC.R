library(dplyr)
library(tidyverse)
library(pxR)


IMC <- readLines("INPUT/DATA/Indice-masa-corporal.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento

IMC_utf8 <- iconv(IMC, from = "ISO-8859-1", to = "UTF-8")

archivo_utf8 <- tempfile(fileext = ".px")
writeLines(IMC_utf8, archivo_utf8)

# Lee el archivo temporal con read.px
datos_IMC <- read.px(archivo_utf8)

#Este dataframe tiene mal los nombres.
DF_mal <- as.data.frame(datos_IMC)
DF_mal
#view(DF_mal)


#Cambiar de anios a años y de BÃ¡sico a básico
DF <- DF_mal %>%
  transmute(
    Masa.corporal.adultos = Masa.corporal.adultos,
    Nivel.de.estudios = factor(case_when(
      Nivel.de.estudios == "TOTAL" ~ "TOTAL",
      Nivel.de.estudios == "BÃ¡sico e inferior" ~ "Básico e inferior",
      Nivel.de.estudios == "Intermedio" ~ "Intermedio",
      Nivel.de.estudios == "Superior" ~ "Superior",
    )),
    Edad = factor(case_when(
      Edad == "TOTAL" ~ "TOTAL",
      Edad == "De 18 a 24 aÃ±os" ~ "De 18 a 24 años",
      Edad == "De 25 a 64 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 65 y mÃ¡s aÃ±os" ~ "De 65 y más años",
    )),
    Sexo = Sexo,
    Porcentaje.personas = value
  )
#view(DF)


#Eliminar las 20 primeras filas que salen mal
datos_IMC_df <- DF[-(1:20), ]
datos_IMC_df
#view(datos_IMC_df)



#Datos sin ambos sexos, sin los totales de edad y estudios y con los datos de obesidad
datos_obesidad <- datos_IMC_df %>%
  filter(
    Masa.corporal.adultos == "Obesidad (IMC>=30 kg/m2)",    
    Sexo != "Ambos sexos",                                  
    Edad != "TOTAL",                                        
    Nivel.de.estudios != "TOTAL"                            
  )
#View(datos_obesidad)


#DIFERENTES CÁLCULOS SIN IMPORTANCIO
#La media del porcentaje de obesidad es la siguiente
media_obesidad <- mean(datos_obesidad$Porcentaje.personas, na.rm = TRUE)
print(media_obesidad)

#La media de obesidad por hombres y mujeres es la siguiente
datos_obesidad$Sexo <- droplevels(datos_obesidad$Sexo)#Al tener en los datos iniciales la columna
#Sexo con los niveles ambos sexos, hombres y mujeres, tenemos que eliminar el nivel ambos sexos,
#y droplevels elimina los niveles que no se usan en el dataframe.
media_por_sexo <- tapply(datos_obesidad$Porcentaje.personas, datos_obesidad$Sexo, mean, na.rm = TRUE)
print(media_por_sexo)


#La media de obesidad por nivel de estudios
datos_obesidad$Nivel.de.estudios <- droplevels(datos_obesidad$Nivel.de.estudios)
#El droplevels para eliminar el nivel TOTAL
media_por_estudios <- tapply(datos_obesidad$Porcentaje.personas, datos_obesidad$Nivel.de.estudios, mean, na.rm = TRUE)
print(media_por_estudios)


#La media de obesidad por edad
datos_obesidad$Edad <- droplevels(datos_obesidad$Edad)
#El droplevels para eliminar el nivel TOTAL
media_por_edad <- tapply(datos_obesidad$Porcentaje.personas, datos_obesidad$Edad, mean, na.rm = TRUE)
print(media_por_edad)









#CREACIÓN GRÁFICA POLAR QUE REPRESENTA EL PORCENTAJE QUE HAY DE PESOS

datos_IMC_grafica3 <- datos_IMC_df %>%
  filter(
    Sexo != "Ambos sexos",                                  # Excluir ambos sexos
    Edad != "TOTAL",                                        # Excluir edad total
    Nivel.de.estudios != "TOTAL",                           # Excluir nivel de estudios total
    Masa.corporal.adultos != "TOTAL"
  )

#View(datos_IMC_grafica2)

#Eliminamos ahora los niveles que no utilizamos para que luego en la gráfica no aparezcan
datos_IMC_grafica3$Sexo <- droplevels(datos_IMC_grafica3$Sexo)
datos_IMC_grafica3$Edad <- droplevels(datos_IMC_grafica3$Edad)
datos_IMC_grafica3$Nivel.de.estudios <- droplevels(datos_IMC_grafica3$Nivel.de.estudios)
datos_IMC_grafica3$Masa.corporal.adultos <- droplevels(datos_IMC_grafica3$Masa.corporal.adultos)

IMC_grafica3_sin_niveles <- datos_IMC_grafica3 %>%
  group_by(Masa.corporal.adultos) %>%
  dplyr::summarise(Porcentaje.personas = mean(Porcentaje.personas, na.rm = TRUE))

#View(IMC_grafica3_sin_niveles)

ggplot(data = IMC_grafica3_sin_niveles) + 
  geom_bar(
    mapping = aes(x = Masa.corporal.adultos, y=Porcentaje.personas,fill = Masa.corporal.adultos),
    stat = "identity",  # Para que se usen los valores de y que ya están calculados, si no da error porque intenta buscar categorías de masa.corporal.adultos
    show.legend = TRUE,
    width = 1
  ) + 
  scale_fill_manual(
    values = c(
      "Peso insuficiente (IMC<18,5 kg/m2)" = "red",
      "Normopeso (18,5 kg/m2 <=IMC<25 kg/m2)" = "green",
      "Sobrepeso (25 kg/m2 <=IMC<30 kg/m2)" = "orange",
      "Obesidad (IMC>=30 kg/m2)" = "blue"
    )
  ) +
  theme(aspect.ratio = 1) +
  labs(
    title="Porcentaje de pesos",
    x = "Masa corporal de adultos", 
    fill = "Masa corporal de adultos") +
  coord_polar()+
  theme(axis.text.x = element_blank()) # Para que no se muestre la leyenda dentro del gráfico.















#GRÁFICO QUE RELACIONA OBESIDAD, NIVEL DE ESTUDIOS, SEXO Y EDAD
media_por_grupo <- datos_obesidad %>%
  group_by(Nivel.de.estudios, Sexo, Edad) %>%
  dplyr::summarise(media_porcentaje = mean(Porcentaje.personas, na.rm = TRUE))

print(media_por_grupo)
str(media_por_grupo)
#View(media_por_grupo5)

ggplot(media_por_grupo, aes(x = Nivel.de.estudios, y = media_porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Edad) +  # Dividir por grupos de edad
  labs(
    title = "Distribución por Nivel de Estudios, Edad y Sexo",
    x = "Nivel de estudios",
    y = "Porcentaje de personas"
  ) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para que se vean bien
  )















#GRÁFICA DE OBESIDAD Y PRODUCTOS AZUCARADOS
Alimentacion_IMC <- left_join(
  datos_Alimentacion,  # Primer DataFrame
  datos_IMC_df,
  by = join_by(Edad, Sexo)# Segundo DataFrame  ,  # Tipo de join: "inner"  # Para diferenciar las columnas con el mismo nombre
)
#View(Alimentacion_IMC)

Alimentacion_IMC_sin_na<-Alimentacion_IMC%>%
  drop_na()
#View(Alimentacion_IMC_sin_na)

Alim_IMC_filtrados1<- Alimentacion_IMC_sin_na %>%
  drop_na() %>%
  filter(
    Sexo != "Ambos sexos",                                  
    Edad != "TOTAL",                                        
    Nivel.de.estudios != "TOTAL",                           
    Frecuencia != "TOTAL",
    Frecuencia =="A diario",
    Alimentos == "Comida rápida" |
    Alimentos == "Dulces" |
    Alimentos == "Refrescos con azúcar",
    Porcentaje.personas != 100,
    Masa.corporal.adultos=="Obesidad (IMC>=30 kg/m2)"
  ) %>%
  group_by(Edad, Frecuencia, Alimentos, Sexo, Masa.corporal.adultos, Nivel.de.estudios) 

#View(Alim_IMC_filtrados1)

ggplot(data = Alim_IMC_filtrados1, aes(x = Porcentaje.personas, y = value)) +
  geom_point(aes(colour = factor(Sexo)))+
  geom_smooth(method = "loess", colour = "blue", se = TRUE) +
  labs(
    title = "Relación obesidad y consumo diario de productos azucarados",
    x="Porcentajes de personas con obesidad",
    y="Porcentaje de personas que consumen productos azucarados a diario",
    colour="Sexo"
  )










#INTENTO GRÁFICA DESEMPLEO Y IMC
#View(Desempleo_data)

Desempleo_IMC <- left_join(
  datos_IMC_df,
  Desempleo_data, 
  by = join_by(Edad, Sexo))
#View(Desempleo_IMC)

Desmepleo_IMC_filtrado<- Desempleo_IMC%>%
  drop_na()%>%
  filter(
    #Tiempo_de_búsqueda_de_empleo=="2 años o más",
    Masa.corporal.adultos=="Obesidad (IMC>=30 kg/m2)",
    Masa.corporal.adultos!="TOTAL",
    Nivel.de.estudios!="TOTAL"
    
  )
  #group_by(Tiempo_de_búsqueda_de_empleo,Edad)%>%
  #summarise(media = mean(Porcentaje.personas, na.rm = TRUE))

#View(Desmepleo_IMC_filtrado)
#Esta gráfica no tiene sentido, no poner
ggplot(data = Desmepleo_IMC_filtrado, aes(x = Porcentaje.personas, y = value)) +
  geom_point(aes(colour = factor(Sexo)))+
  geom_smooth(method = "loess", colour = "blue", se = TRUE) +
  labs(
    title = "Relación obesidad y desempleo",
    x="Porcentajes de personas con obesidad",
    y="Miles de personas en desempleo",
    colour="Sexo"
  )



ggplot(data = Desmepleo_IMC_filtrado, aes(x = Tiempo_de_búsqueda_de_empleo, y = value)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = factor(Sexo))) +
  facet_wrap(~ Masa.corporal.adultos) +  # Dividir por grupos de edad
  labs(
    title = "Obesidad y desempleo",
    x = "Porcentaje de personas",
    y = "Miles de personas en desempleo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje x para que se vean bien
  )
  

