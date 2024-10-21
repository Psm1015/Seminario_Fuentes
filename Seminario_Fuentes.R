#Script para el seminario de fuentes
library(dplyr)
library(tidyverse)
library(rjson)
library(pxR)
library(jsonlite)

Alimentacion <- fromJSON("INPUT/DATA/Alimentacion.json")#hemos usado IA para solucionar un error. 
#hemos tenido que instalar el paquete jsonlite, ya que con rjson nos salia este erro: 
#Error: unable to find an inherited method for function ‘fromJSON’ for signature ‘content = "missing", handler = "missing"’
#Al utilizar este paquete se utiliza el fromjson de este paquete y hay que quitar el file= del paquete rjson
Indice_masa_corporal <- fromJSON("INPUT/DATA/Indice-masa-corporal.json")
Desempleo <- read.px("INPUT/DATA/Desempleo.px")
