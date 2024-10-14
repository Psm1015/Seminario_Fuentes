#Script para el seminario de fuentes
library(tidyverse)
library(rjson)

Alimentacion <- fromJSON(file="INPUT/DATA/Alimentacion.json")
Indice_masa_corporal <- fromJSON(file="INPUT/DATA/Indice-masa-corporal.json")

