#Script para el seminario de fuentes
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(jsonlite)

IMC<- fromJSON("INPUT/DATA/Indice-masa-corporal.json")
spread_all(IMC)
view(IMC)
