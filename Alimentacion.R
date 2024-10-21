library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(jsonlite)

Alimentacion <- fromJSON("INPUT/DATA/Alimentacion.json") 
str(Alimentacion)
view(Alimentacion)
