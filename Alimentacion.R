library(dplyr)
library(tidyverse)
library(pxR)


Alimentacion <- readLines("INPUT/DATA/Alimentacion.px", encoding = "ISO-8859-1")

Alimentacion_utf8 <- iconv(Alimentacion, from = "ISO-8859-1", to = "UTF-8")

str(Alimentacion)
#Alimentacion <- readLines("INPUT/DATA/Alimentacion.px", encoding = "ISO-8859-1") #da un warning, ignorar por el momento
#archivo_texto_utf8 <- iconv(Alimentacion, from = "ISO-8859-1", to = "UTF-8")

archivo_Alimentacion <- tempfile(fileext = ".px")
writeLines(Alimentacion_utf8, archivo_Alimentacion)

# Lee el archivo temporal con read.px
Alimentacion_datos<- read.px(archivo_Alimentacion)

#str(datos$Edad)

# Ver la estructura del archivo cargado
# str(Desempleo)
#a <-unlist(Desempleo)
#a
# Convertir a un data frame para trabajar con los datos

#Rango de edades: 18 a 24, 25 a 64, 65 o más


DF <- as.data.frame(Alimentacion_datos)
DF
#view(DF)

# Filter para quitar diversos rangos de edad
DF1 <- DF %>%
  filter(!(Edad %in% c("De 25 a 34 aÃ±os", "De 45 a 54 aÃ±os", "De 55 a 64 aÃ±os", "De 75 y mÃ¡s aÃ±os")))
DF1
#view(DF1)

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

# Cambiar los rangos de edad en la columna Edad.
datos_df1 <- DF1 %>%
  transmute(
    Edad = case_when(
      Edad == "De 15 a 24 aÃ±os" ~ "De 18 a 24 años",
      Edad == "De 35 a 44 aÃ±os" ~ "De 25 a 64 años",
      Edad == "De 65 a 74 aÃ±os" ~ "De 65 y más años",
    ),
    Frecuencia = case_when(
      Frecuencia == "3 o mÃ¡s veces a la semana pero no a diario" ~ "3 o más veces a la semana pero no a diario",
      Frecuencia == "Menos de 1 vez a la semana" ~ "Menos de 1 vez a la semana",
      Frecuencia == "1 o 2 veces a la semana" ~ "1 o 2 veces a la semana",
      Frecuencia == "A diario" ~ "A diario",
      Frecuencia == "TOTAL" ~ "TOTAL",
      Frecuencia == "Nunca" ~ "Nunca"
    ),
    Alimentos = case_when(
      Alimentos == "Fruta fresca (excluye zumos)" ~ "Fruta fresca (excluye zumos)",
      Alimentos == "Carne" ~ "Carne",
      Alimentos == "Huevos" ~ "Huevos",
      Alimentos == "Pescado" ~ "Pescado",
      Alimentos == "Pasta,arroz,patatas" ~ "Pasta,arroz,patatas",
      Alimentos == "Pan,cereales" ~ "Pan,cereales",
      Alimentos == "Verduras, ensaladas y hortalizas" ~ "Verduras, ensaladas y hortalizas",
      Alimentos == "Legumbres" ~ "Legumbres",
      Alimentos == "Embutidos y fiambres" ~ "Embutidos y fiambres",
      Alimentos == "Productos lÃ¡cteos" ~ "Productos lácteos",
      Alimentos == "Dulces" ~ "Dulces",
      Alimentos == "Refrescos con azÃºcar" ~ "Refrescos con azúcar",
      Alimentos == "Comida rÃ¡pida" ~ "Comida rápida",
      Alimentos == "Aperitivos o comidas saladas de picar" ~ "Aperitivos o comidas saladas de picar",
      Alimentos == "Zumo natural de frutas o verduras" ~ "Zumo natural de frutas o verduras"
    ),
    Sexo = Sexo,
    value = value
  )
#view(datos_df1)

# ELIMINAR TODAS LAS FILAS QUE TENGAN NA EN LA COLUMNA Edad
df_sin_NA <- datos_df1 %>% 
  drop_na()
df_sin_NA
#view(df_sin_NA)


datos_Alimentacion <-  df_sin_NA%>%
  filter(
    Frecuencia != "TOTAL",                                  
  )
#view(datos_Alimentacion)

# GRÁFICA QUE RELACIONA QUE COME CADA RANGO DE EDAD A DIARIO.
library(ggplot2)
Alimentacion_ADiario <- datos_Alimentacion %>%
  filter(
    Frecuencia == "A diario",
    (!(Alimentos %in% c("Aperitivos o comidas saladas de picar", "Zumo natural de frutas o verduras")))
 
  )
Alimentacion_ADiario <- datos_Alimentacion %>%
  filter(
    Frecuencia == "A diario",
    Alimentos %in% c("Carne", "Comida rápida", "Dulces", "Fruta fresca(excluye zumos", "Pescado",
                 "Refrescos con azúcar", "Verduras, ensaladas y hortalizas"))
#view(Alimentacion_ADiario)

ggplot(Alimentacion_ADiario, aes(x = Edad, y = value, fill = Alimentos)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Sexo) +
  labs(title = "Porcentaje de Consumo Diario de Alimentos por Rango de Edad y Sexo",
       x = "Rango de Edad",
       y = "Porcentaje de Consumo Diario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


Alimentacion_ADiario <- datos_Alimentacion %>%
  filter(
    Frecuencia == "A diario",
    (!(Alimentos %in% c("Aperitivos o comidas saladas de picar", "Zumo natural de frutas o verduras")))
    
  )
# Para un gráfico de barras donde 'value' es la altura de las barras y 'Edad' es el eje x
Grafica_AlimentosXDia<-ggplot(data = Alimentacion_ADiario, aes(x = Edad, y = value)) +
  geom_bar(aes(fill = Alimentos), position = "dodge", stat = "identity")  # Añadir stat = "identity"

Grafica_AlimentosXDia
# Guardar este gráfico en OUTPUT/Figures/Alimentacion
ggsave(
  filename = "Alimentos_A_Diario.jpeg",
  plot = Grafica_AlimentosXDia ,
  path = "OUTPUT/Figures/Alimentacion", 
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)


# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME PESCADO 
# 3 O MÁS VECES A LA SEMANA PERO NO A DIARIO
Alimentacion_pescado <- datos_Alimentacion %>%
  filter(
    Frecuencia == "3 o más veces a la semana pero no a diario",
    Alimentos == "Pescado"
  )
#view (Alimentacion_pescado)

Grafica_Pescado <- ggplot(Alimentacion_pescado, aes(x = Edad, y = value, fill = Sexo)) +
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
Grafica_Pescado
ggsave(
  filename = "Pescado_3oMasVeces.jpeg",
  plot = Grafica_Pescado ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Alimentacion", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME CARNE 
# 3 O MÁS VECES A LA SEMANA PERO NO A DIARIO
Alimentacion_carne <- datos_Alimentacion %>%
  filter(
    Frecuencia == "3 o más veces a la semana pero no a diario",
    Alimentos == "Carne"
  )
#view(Alimentacion_carne)
Grafica_Carne <- ggplot(Alimentacion_carne, aes(x = Edad, y = value, fill = Sexo)) +
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
Grafica_Carne

ggsave(
  filename = "Carne_3oMasVeces.jpeg",
  plot = Grafica_Carne ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Alimentacion", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

# GRÁFICA QUE RELACIONA EL PORCENTAJE DE PERSONAS POR SEXO Y RANGO DE EDAD QUE COME COMIDA RAPIDA  
# 1 O 2 VECES A LA SEMANA
Alimentacion_rapida <- datos_Alimentacion %>%
  filter(
    Frecuencia == "1 o 2 veces a la semana",
    Alimentos == "Comida rápida"
  )
#view(Alimentacion_rapida)
Grafica_CRapida <- ggplot(Alimentacion_rapida, aes(x = Edad, y = value, fill = Sexo)) +
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

Grafica_CRapida

ggsave(
  filename = "Comida_Rapida_3oMAsVeces.jpeg",
  plot = Grafica_CRapida ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Alimentacion", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

Alimentacion_IMC <- left_join(
  Alimentacion_rapida,  
  datos_IMC_df,
  by = join_by(Edad, Sexo)
)
#view(Alimentacion_IMC)


# Solo queremos ver las filas donde las personas sean obesas (Obesidad (IMC>=30 kg/m2))
C.Rapida_Obesidad <- Alimentacion_IMC %>%
  filter(
    Masa.corporal.adultos == "Obesidad (IMC>=30 kg/m2)",    # Solo obesidad
    Sexo != "Ambos sexos",                                  # Excluir ambos sexos
    Edad != "TOTAL",                                        # Excluir edad total
    Nivel.de.estudios != "TOTAL")                           # Excluir nivel de estudios total
#View(C.Rapida_Obesidad)

Grafica_CRapida_IMC <- ggplot(C.Rapida_Obesidad, aes(x = Edad, y = Porcentaje.personas, fill = Edad)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar() + 
  theme_minimal() + 
  labs(
    title = "Porcentaje de personas con obesidad\nsegún nivel de estudios y sexo",
    x = NULL,
    y = NULL,
    fill = "Edad"
  ) +
  facet_wrap(~Sexo)   # Crear gráficos separados por sexo
#  theme(axis.text.x = element_blank()) 

Grafica_CRapida_IMC
ggsave(
  filename = "ComidaRapida_Vs_IMC.jpeg",
  plot = Grafica_CRapida_IMC ,
  #path = paste(getwd(), "/OUTPUT/Figures", sep = ""), # ruta absoluta
  path = "OUTPUT/Figures/Alimentacion", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 20,
  units = "cm",
  dpi = 320
)

C.Rapida_Obesidad2 <- C.Rapida_Obesidad %>%
  select(Edad,Sexo, value, Porcentaje.personas)
#view(C.Rapida_Obesidad2)

library(dplyr)
Obesidad_CRapida <- C.Rapida_Obesidad2 %>% 
  as_tibble() %>% 
  group_by(Edad, Sexo, value) %>%  # Agrupar por Edad y Sexo
  dplyr::summarise(Media_Porcentaje.personas = mean(Porcentaje.personas, na.rm = TRUE))

#view(Obesidad_CRapida)


C.Rapida_Obesidad3 <- C.Rapida_Obesidad %>%
  select(Sexo, value, Nivel.de.estudios, Porcentaje.personas)
#view(C.Rapida_Obesidad3)

Obesidad_CRapida2 <- C.Rapida_Obesidad3 %>% 
  as_tibble() %>% 
  group_by(Sexo, Nivel.de.estudios) %>%  # Agrupar por Edad y Sexo
  dplyr::summarise(Media_Porcentaje.personas = mean(Porcentaje.personas, na.rm = TRUE),
                   Media_Valor = mean(value, na.rm = TRUE))
#view(Obesidad_CRapida2)

ggplot(data = Obesidad_CRapida2, aes(x = Nivel.de.estudios, y = Media_Porcentaje.personas)) +
  geom_point(aes(colour = factor(Sexo), shape = factor(Media_Valor)))



library(dplyr)
Porcentaje_alimentacion <- datos_Alimentacion %>% 
  as_tibble() %>% 
  filter(Sexo != "Ambos sexos")%>%
  group_by(Alimentos, Sexo) %>%  # Agrupar por Edad y Sexo
  dplyr::summarise(Media_valor = mean(value, na.rm = TRUE))
view(Porcentaje_alimentacion)

IMC_Vs_Alimentacion <- left_join(
  datos_Alimentacion,
  datos_IMC_df,
  by = join_by(Edad, Sexo)
) 

view(IMC_Vs_Alimentacion)

IMC_Vs_CRapida <- IMC_Vs_Alimentacion %>%
  filter(Sexo != "Ambos sexos",                                  
         Edad != "TOTAL",                                        
         Nivel.de.estudios != "TOTAL",                           
         Frecuencia != "TOTAL",
         Alimentos == "Comida rápida")

view(IMC_Vs_Alimentacion)





