#Script para el seminario de fuentes
library(dplyr)
library(tidyverse)
library(tidyr)
library(tidyjson)
library(rjson)
library(RJSONIO)


#prueba<-fromJSON("INPUT/DATA/contratos-agroambiente-clima-sac.json")
#str(prueba)
#View(prueba)


?fromJSON
IMC<-fromJSON("INPUT/DATA/Indice-masa-corporal.json")
str(IMC)
View(IMC)
spread_all(IMC)

#Pruebas para poner bien los datos.
IMC %>% 
  spread_all() %>% 
  gather_object %>% 
  json_types %>% 
  count(name, type)

IMC %>%
  enter_object(Data) %>%
  gather_array %>%
  spread_all %>%
  select(-document.id, -array.index)

#Esto codigo ns si está bien
IMC %>%
  spread_all() %>%         # Expande todos los elementos al nivel principal
  enter_object("Data") %>% # Entra al objeto "Data" para procesarlo
  gather_array() %>%       # Extrae los elementos del array "Data"
  spread_all() %>%         # Expande los elementos de "Data" si contienen objetos
  bind_rows(               # Combina con otros elementos si fuera necesario
    IMC %>%
      enter_object("MetaData") %>%
      gather_array() %>%
      spread_all()
  ) %>%
  select(-document.id, -array.index)



# Paso 1: Expande el objeto principal IMC (donde están "Nombre", "MetaData", "Data")
IMC_expanded <- IMC %>%
  spread_all()  # Expande los campos principales (Nombre, MetaData, Data)

expanded_json <- IMC_expanded %>%
  select(document.id, Nombre, ..JSON) %>%    # Seleccionamos las columnas relevantes
  enter_object("..JSON") %>%                  # Entra en la columna que contiene objetos JSON
  spread_all()                                # Expande todos los elementos dentro del JSON

# Verifica la expansión
head(expanded_json)

IMC_expanded %>% 
  gather_object("..JSON") %>% 
  json_types()

final_result <- expanded_json %>%
  select(-document.id) %>%
  bind_cols(
    IMC_expanded %>% select(document.id)  # Incluimos de nuevo `document.id` si es necesario
  )

# Ver el resultado final
View(final_result)


# Verificar la estructura después de la expansión
# Esto es solo para fines de verificación
head(IMC_expanded)

# Paso 2: Expande el objeto MetaData (que contiene variables como "T3_Variable", "Nombre", "Codigo")
MetaData_expanded <- IMC_expanded %>%
  enter_object("..JSON") %>%
  gather_array() %>%
  spread_all()  # Expande los subcomponentes de MetaData

Nuevo_expanded <-  MetaData_expanded%>%
  enter_object("..JSON") %>%
  gather_array() %>%
  spread_all()  # Expande los subcomponentes de MetaData

View(Nuevo_expanded)
# Verifica los datos de MetaData después de la expansión
# Esto es solo para fines de verificación
head(MetaData_expanded)

# Paso 3: Expande el objeto Data (donde están los valores numéricos)
Data_expanded <- IMC_expanded %>%
  enter_object("Data") %>%
  gather_array() %>%
  spread_all()  # Expande los valores numéricos de Data

# Verifica los datos de Data después de la expansión
# Esto es solo para fines de verificación
head(Data_expanded)

# Paso 4: Combina los datos de MetaData y Data en un solo dataframe
final_result <- IMC_expanded %>%
  select(Nombre) %>%  # Seleccionamos la columna 'Nombre' (como encabezado o categoría)
  bind_cols(MetaData_expanded) %>%  # Combina MetaData con los valores de Nombre
  bind_cols(Data_expanded)  # Combina los valores numéricos de Data

# Ver el resultado final
View(final_result)
View(IMC_expanded)
colnames(IMC_expanded)




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
