#Script para el seminario de fuentes
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(RJSONIO)


prueba<-fromJSON("INPUT/DATA/contratos-agroambiente-clima-sac.json")
str(prueba)
View(prueba)


?fromJSON
IMC<-fromJSON("INPUT/DATA/Indice-masa-corporal.json")
str(IMC)
View(IMC)
spread_all(IMC)

glimpse(IMC)
glimpse(unlist(IMC$MetaData))
a <- unlist(IMC$MetaData)
spread_all(unlist(IMC$MetaData))
print(a)



spread_all(IMC, json.column = "MetaData")
view(IMC)
IMC %>% 
     spread_all() %>% 
     View()
IMC %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)
str(IMC)



# Verificar los cambios
str(IMC$MetaData)
names(IMC$MetaData[[1]])
