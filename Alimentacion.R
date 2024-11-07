library(dplyr)
library(tidyverse)
library(tidyr)
library(purrr)
library(tidyjson)
library(rjson)
library(RJSONIO)


Alimentacion <- fromJSON("INPUT/DATA/Alimentacion.json") 
str(Alimentacion)
view(Alimentacion)
head(Alimentacion)

Alimentacion %>% 
  spread_all() %>% 
  View()

Alimentacion %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

resultado <- Alimentacion %>%
  enter_object("Data") %>%
  gather_array() %>%
  unnest_wider(json.column = ..JSON) %>%  # Especificamos la columna correcta
  enter_object("MetaData") %>%
  gather_array() %>%
  unnest_wider(json.column = ..JSON) %>%  # Igual para MetaData
  select(-document.id, -array.index)  # Eliminar columnas innecesarias

# Inspeccionar el resultado
glimpse(resultado)


# Inspeccionar la estructura del resultado
glimpse(resultado)

# Inspeccionar la estructura del resultado
glimpse(resultado)

# Inspeccionar la estructura de los datos antes de usar View()
glimpse(resultado)  # Muestra una vista compacta de las primeras filas
head(resultado)  # Muestra las primeras filas de los datos

Alimentacion_na %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

Alimentacion %>% enter_object(MetaData) %>% glimpse()

Alimentacion %>%
  enter_object(MetaData) %>%
  unnest_wider(..JSON, names_sep = "_") %>%  # Generar nombres automáticos con el separador "_"
  select(-document.id) %>%  # Eliminar document.id si no lo necesitas
  view()  # Mostrar los resultados
  
# Cambiar los rangos de edad
Alimentacion_full <- Alimentacion %>%
  mutate(MetaData = case_when(
    MetaData %in% c('De 15 a 24 años', 'De 18 a 24 años') ~ '18 a 24',
    MetaData %in% c('De 25 a 34 años', 'De 35 a 44 años', 'De 45 a 54 años', 'De 55 a 64 años') ~ '25 a 64',
    MetaData %in% c('De 65 a 74 años', 'De 75 años o más') ~ '65 o más',
    TRUE ~ MetaData # Mantener el valor actual si no coincide con ningún rango
  ))

# Ver la estructura de los datos después de la modificación
str(Alimentacion_full)

# Ver algunos resultados para confirmar el cambio
View(Alimentacion_full)
