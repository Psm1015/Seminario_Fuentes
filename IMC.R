library(dplyr)
library(tidyverse)
library(pxR)




#PRIMERO OBTENEMOS LOS DATOS EN FORMATO DE DATAFRAME

#Leemos linea a linea el contenido del archivo.encoding es la codififcación dela rchivo original
IMC <- readLines("INPUT/DATA/Indice-masa-corporal.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento

#Convertimos el archivo a UTF-8.
IMC_utf8 <- iconv(IMC, from = "ISO-8859-1", to = "UTF-8")

#Crea un archivo temporal con extensión px.
archivo_temp_IMC <- tempfile(fileext = ".px")

#Para escribir el contenido de IMC_utf8 en el archivo temporal.
writeLines(IMC_utf8, archivo_temp_IMC)

# Lee el archivo temporal con read.px
datos_IMC <- read.px(archivo_temp_IMC)

#Este dataframe tiene mal los nombres.
DF_mal <- as.data.frame(datos_IMC)
#DF_mal
#view(DF_mal)





#UNA VEZ TENEMOS EL DATAFRAME, LO MODIFICAMOS PARA HACERLO APTO PARA EL USO

#Cambiar de anios a años y de BÃ¡sico a básico
DF_IMC <- DF_mal %>%
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

#Datos sin ambos sexos, sin los totales de edad y estudios.
datos_IMC_df <- DF_IMC %>%
  drop_na()%>%
  filter(
    Masa.corporal.adultos != "TOTAL",    
    Sexo != "Ambos sexos",                                  
    Edad != "TOTAL",                                        
    Nivel.de.estudios != "TOTAL"                            
  )
#view(datos_IMC_df)

#Al tener en los datos iniciales la columna Sexo con los niveles ambos sexos, 
#hombres y mujeres, tenemos que eliminar el nivel ambos sexos,
#y droplevels elimina los niveles que no se usan en el dataframe.
datos_IMC_df$Sexo <- droplevels(datos_IMC_df$Sexo)

#Hacemos lo mismo con el resto de columnas en las que no se usan el nivel "TOTAL"
datos_IMC_df$Nivel.de.estudios <- droplevels(datos_IMC_df$Nivel.de.estudios)
datos_IMC_df$Edad <- droplevels(datos_IMC_df$Edad)
datos_IMC_df$Masa.corporal.adultos <- droplevels(datos_IMC_df$Masa.corporal.adultos)

#Datos específicos de obesidad
datos_obesidad <- datos_IMC_df %>%
  filter(
    Masa.corporal.adultos == "Obesidad (IMC>=30 kg/m2)",    
  )
#View(datos_obesidad)








#CREACIÓN GRÁFICA POLAR QUE REPRESENTA EL PORCENTAJE QUE HAY DE PESOS

IMC_grafica1<- datos_IMC_df %>%
  group_by(Masa.corporal.adultos) %>%
  dplyr::summarise(Porcentaje.personas = mean(Porcentaje.personas, na.rm = TRUE))

#View(IMC_grafica1)

Porcentaje_pesos<-ggplot(data = IMC_grafica1) + 
  geom_bar(
    mapping = aes(x = Masa.corporal.adultos, y=Porcentaje.personas,fill = Masa.corporal.adultos),
    stat = "identity",  # Para que se usen los valores de y que ya están calculados, si no da error porque intenta buscar categorías de masa.corporal.adultos
    show.legend = TRUE,
    width = 1 #Sin esto no salen bien cuadrados los diferentes triángulos, ya 
    #que se queda un espacio en blanco entre ellos, y al ponerlo salen pegados.
  ) + 
  scale_fill_manual( #Sin ponerlo manual los colores no salían como queríamos.
    values = c(
      "Peso insuficiente (IMC<18,5 kg/m2)" = "red",
      "Normopeso (18,5 kg/m2 <=IMC<25 kg/m2)" = "green",
      "Sobrepeso (25 kg/m2 <=IMC<30 kg/m2)" = "orange",
      "Obesidad (IMC>=30 kg/m2)" = "blue"
    )
  ) +
  labs(
    title="Porcentaje de pesos",
    x = "Masa corporal de adultos", 
    fill = "Masa corporal de adultos") +
  coord_polar()+
  theme(axis.text.x = element_blank()) # Para que no se muestre la leyenda 
  #dentro del gráfico, ya que no salen bien.

Porcentaje_pesos

#Guardamos la gráfica
ggsave(
  filename = "Porcentajes_pesos.jpeg",
  plot = Porcentaje_pesos ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/IMC", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)








#GRÁFICO QUE RELACIONA OBESIDAD, NIVEL DE ESTUDIOS, SEXO Y EDAD

media_por_grupo <- datos_obesidad %>%
  group_by(Nivel.de.estudios, Sexo, Edad) %>%
  dplyr::summarise(media_porcentaje = mean(Porcentaje.personas, na.rm = TRUE))

print(media_por_grupo)
str(media_por_grupo)
#View(media_por_grupo5)

Relaciones_obesidad<-ggplot(media_por_grupo, aes(x = Nivel.de.estudios, y = media_porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") + #Con stat=identity hacemos 
  #que coja los datos ya calculados y con position dodge conseguimos que las 
  #columans de cada sexo salgan al lado y no una encima de otra
  facet_wrap(~ Edad) +  #Para que en el gráfico salgan datos para cada nivel de
  #la columna Edad
  labs(
    title = "Distribución por Nivel de Estudios, Edad y Sexo",
    x = "Nivel de estudios",
    y = "Porcentaje de personas"
  ) +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "salmon")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, , hjust = 1)  # Rotar etiquetas del 
    #eje x para que se vean bien, ya que si no se superponen, y con hjust 
    #hacemos que dichas etiquetas empiecen justo debaje del gráfico ya que sino
    #salen dentro.
  )

Relaciones_obesidad

ggsave(
  filename = "Relaciones obesidad.jpeg",
  plot = Relaciones_obesidad ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/IMC", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)






#GRÁFICA DE OBESIDAD Y PRODUCTOS AZUCARADOS

Alimentacion_IMC <- left_join(
  datos_Alimentacion,  
  datos_IMC_df,
  by = join_by(Edad, Sexo)
)
#View(Alimentacion_IMC)

Alimentacion_IMC_sin_na<-Alimentacion_IMC%>%
  drop_na()
#View(Alimentacion_IMC_sin_na)

Alim_IMC_filtrados1<- Alimentacion_IMC_sin_na %>%
  drop_na() %>%
  filter(
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

Obesidad_azucares<-ggplot(data = Alim_IMC_filtrados1, aes(x = Porcentaje.personas, y = value)) +
  geom_point(aes(colour = factor(Sexo)))+
  geom_smooth(method = "loess", colour = "blue", se = TRUE) +
  labs(
    title = "Relación obesidad y consumo diario de productos azucarados",
    x="Porcentajes de personas con obesidad",
    y="Porcentaje de personas que consumen productos azucarados a diario",
    colour="Sexo"
  )

Obesidad_azucares

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
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje
    #x para que se vean bien
  )
  

