library(dplyr)
library(tidyverse)
library(pxR)

Desempleo <- read.px("INPUT/DATA/Desempleo.px")
# Ver la estructura del archivo cargado
 str(Desempleo)
 a <-unlist(Desempleo)

# Convertir a un data frame para trabajar con los datos
df11 <- as.data.frame(a)
df11
view(df1)
