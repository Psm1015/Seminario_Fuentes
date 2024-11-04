library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(jsonlite)

Alimentacion <- fromJSON("INPUT/DATA/Alimentacion.json") 
str(Alimentacion)
view(Alimentacion)


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
