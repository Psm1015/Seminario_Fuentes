library(dplyr)
library(tidyverse)
library(pxR)

Desempleo <- read.px("INPUT/DATA/Desempleo.px")
# Ver la estructura del archivo cargado
str(Desempleo)

# Convertir a un data frame para trabajar con los datos
df11 <- as.data.frame(Desempleo)
df11
view(df1)
