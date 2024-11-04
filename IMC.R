#Script para el seminario de fuentes
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(jsonlite)

IMC<- fromJSON("INPUT/DATA/Indice-masa-corporal.json")
spread_all(IMC, json.column = "MetaData")
view(IMC)
IMC %>% 
     spread_all() %>% 
     View()
IMC %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)
