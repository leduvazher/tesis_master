install.packages("dplyr")
install.packages("purrr")
install.packages("foreign") 
install.packages("tidyverse")


library(foreign)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)


carpeta <- "C:/Users/eduva/Documents/Tesis/dataset"

# Obtener la lista de archivos dbf en la carpeta

archivos <- list.files(carpeta, pattern = "*.dbf", ignore.case = TRUE)

# Iterar sobre los archivos dbf
for (archivo in archivos) {
  # Leer el archivo dbf
  dataframe <- read.dbf(paste0(carpeta, "/", archivo))
  
  # Darle el nombre del archivo al dataframe
  colnames(dataframe) <- colnames(read.dbf(archivo))
  
  # Guardar el dataframe en un objeto
  assign(archivo, dataframe)
}


###Seleccionar todos los dataframes que comiencen con DEFUN y cargarlos

defun_dataframes <- mget(ls(pattern = "^DEFUN"))

###agregar a todos los dataframes el nombre del archivo para evitar perder nformacion

defun_dataframes_name <- imap(defun_dataframes, ~ .x %>% mutate(FILE_NAME = .y))

###En 2022 cambio el nombre dela columna

defun_dataframes_name$DEFUN22.dbf <- defun_dataframes_name$DEFUN22.dbf %>%   rename(
  PRESUNTO = TIPO_DEFUN
)

defun_filtered <- lapply(defun_dataframes_name, function(df) {
  df %>%
    select(ENT_REGIS, MUN_REGIS, ENT_OCURR, MUN_OCURR, SEXO, MES_OCURR, ANIO_OCUR, 
           MES_REGIS, ANIO_REGIS, EDAD_AGRU, ANIO_NACIM, PRESUNTO, FILE_NAME)
})

###Pegamos todos los dataframes para convertirlo en uno solo

dataframe_final <- do.call(rbind, defun_filtered)

###Creamos la variable edad 

dataframe_final$EDAD <- dataframe_final$ANIO_OCUR - dataframe_final$ANIO_NACIM

#####Data analysis, renombramos para tener un dataframe para pruebas

dataframe_final_stg <- dataframe_final

dataframe_final_stg$ENT_REGIS <- as.numeric(dataframe_final_stg$ENT_REGIS)
dataframe_final_stg$ENT_OCURR <- as.numeric(dataframe_final_stg$ENT_OCURR)


###Sexo:  Replace 99's to not identified

dataframe_final_stg$SEXO  <- case_when(
    dataframe_final_stg$SEXO == 1  ~ "Hombre",
    dataframe_final_stg$SEXO == 2  ~ "Mujer",
    dataframe_final_stg$SEXO == 9  ~ "No especificado",
    .default = "Check"
  )

dataframe_final_stg %>% distinct(SEXO)

###Asignar nombre a Estados 

###ENT_REGIS

catalogo_estados <- read.csv("~/Tesis/dataset/catalogo_entidades.csv", header = TRUE)

dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_REGIS" = "clave"))

dataframe_final_stg <- dataframe_final_stg %>%
  rename(
    ENT_REGIS_NAME = entidad
  )

dataframe_final_stg <- select(dataframe_final_stg, -indice_paz)

###ENT_OCURR

dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_OCURR" = "clave"))

dataframe_final_stg <- dataframe_final_stg %>%
  rename(
    ENT_OCURR_NAME = entidad
  )

dataframe_final_stg <- select(dataframe_final_stg, -indice_paz)

###filtromos los homidicios

dataframe_final_stg %>% distinct(PRESUNTO)

dataframe_final_stg <- filter(dataframe_final_stg, PRESUNTO == 2)

#filter(dataframe_final_stg, ANIO_OCUR == 99)

###Arreglar nombre de las columnas

###ANIO_OCURR

dataframe_final_stg$ANIO_OCUR_FIXED  <- case_when(
  str_trim(dataframe_final_stg$ANIO_OCUR)   == 99  ~ "No especificado",
  str_length(dataframe_final_stg$ANIO_OCUR) == 1   ~ as.character(paste0("190",dataframe_final_stg$ANIO_OCUR)),
  str_length(dataframe_final_stg$ANIO_OCUR) == 2   ~ as.character(paste0("19" ,dataframe_final_stg$ANIO_OCUR, collpase = NULL)),
  str_length(dataframe_final_stg$ANIO_OCUR) == 4   ~ as.character(dataframe_final_stg$ANIO_OCUR),
  .default = "Check"
)

###EN anio_ocurr revisamos que tenemos variables no especificado (codigo 99) 
###y 9999 que tambien es no especficado

dataframe_final_stg$ANIO_OCUR_FIXED[dataframe_final_stg$ANIO_OCUR_FIXED == "9999"] <- "No especificado"

##Validamos

check <- dataframe_final_stg %>% count(ANIO_OCUR_FIXED)
check

###ANIO_REGIS CORREGIR

dataframe_final_stg$ANIO_REGIS_FIXED  <- case_when(
  str_trim(dataframe_final_stg$ANIO_REGIS)   == 99  ~ "No especificado",
  str_length(dataframe_final_stg$ANIO_REGIS) == 1   ~ as.character(paste0("190",dataframe_final_stg$ANIO_REGIS)),
  str_length(dataframe_final_stg$ANIO_REGIS) == 2   ~ as.character(paste0("19" ,dataframe_final_stg$ANIO_REGIS, collpase = NULL)),
  str_length(dataframe_final_stg$ANIO_REGIS) == 4   ~ as.character(dataframe_final_stg$ANIO_REGIS),
  .default = "Check"
)
###Validamos

check <- dataframe_final_stg %>% count(ANIO_REGIS_FIXED)
check

####EDAD##### AÑO DE OCURRENCIA MENOS AÑO DE NACIMIENTO ** OJO PUEDE HABER NO ESPECIFICADOS ***
###PUEDE HABER CASOS DONDE EL AÑO DE OCURRIA ES NO ESPECIFICADO

dataframe_final_stg$EDAD_FIXED  <-        case_when(
  dataframe_final_stg$EDAD   <  0  ~ "No especificado",
  dataframe_final_stg$EDAD   > 120 ~ "No especificado",
  .default = as.character(dataframe_final_stg$EDAD)
)


check_edad <- dataframe_final_stg %>% count(EDAD_FIXED)
check_edad

dataframe_final_stg$EDAD_FIXED_GROUP  <- if_else(dataframe_final_stg$EDAD_FIXED == "No especificado","No especificado", 
                                                 if_else(dataframe_final_stg$EDAD_FIXED < 18 , "Adulto", "No adulto" ))

check_edad <- dataframe_final_stg %>% count(EDAD_FIXED_GROUP)
check_edad

###### AGREGAMOS COLUMNAS DEL INDICE DE PAZ

catalogo_estados$clave <- as.numeric(catalogo_estados$clave)

dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_REGIS" = "clave"))

dataframe_final_stg <- select(dataframe_final_stg, -c(entidad))

check_ent_ocur <- dataframe_final_stg %>% count(ENT_OCURR)
check_ent_ocur

filter(dataframe_final_stg, ENT_OCURR == 33)

##34 es para otros paises de latinoamerica
##33 Estados Unidos de America


###Data analysis
###Filtros para aplicar al modelo
###1. Variable ANIO_OCURRUR: -Filtrar todos los homocidios menos a 1990
###                          -Filtrar lo no especificado
###2. Mes Ocurr:             -Filtrar homocidios no especificados
###                          -Filtrar registros en blanco
###3. Correr script para validar si existen valores no especificados
###4. Edad Fixed             -Filtrar registros no especificados
###5. Indice de paz          -Borrar Nulls



###-Filtrar todos los homocidios menos a 1990 y lo no especificado


dataframe_final_stg_test <- filter(dataframe_final_stg, ANIO_OCUR_FIXED > 1989 & ANIO_OCUR_FIXED != "No especificado")

dataframe_final_stg_test %>% distinct(ANIO_OCUR_FIXED) 

dataframe_final_stg <- filter(dataframe_final_stg, ANIO_OCUR_FIXED > 1989 & ANIO_OCUR_FIXED != "No especificado")

###Filtrar mes no especificados

dataframe_final_stg %>% distinct(MES_OCURR)  ##Revisamos la data de los homocidios no especificados
dataframe_final_stg %>% count(MES_OCURR)

dataframe_final_stg_test %>% filter(dataframe_final_stg, MES_OCURR != "99")
dataframe_final_stg %>% distinct(MES_OCURR)




subset(dataframe_final_stg,ENT_REGIS == 16 & MUN_REGIS == 108 & 
         ENT_OCURR == 16 & MUN_OCURR == 108 & SEXO == "Hombre,")


dataframe_final_stg %>% distinct(ANIO_OCUR_FIXED)

dataframe_final_stg <- filter(dataframe_final_stg, ANIO_OCUR_FIXED == 2)







check_edad <- dataframe_final_stg %>% count(EDAD_FIXED_GROUP)
check_edad


###Data Analysis Numero 1


          

check <- dataframe_final_stg %>% count(ANIO_OCUR_FIXED)
check




#final

df_hom <- dataframe_final_stg

####GROUP BY datasets

df_hom_grouped <-    df_hom %>%
                        group_by(ANIO_OCUR,MES_OCURR,EDAD_FIXED_GROUP,indice_paz) %>%
                        summarise(n = n())


df_hom_grouped


as.Date(paste(df_hom_grouped$ANIO_OCUR_FIXED, df_hom_grouped$MES_OCURR, "01", sep = "-"),format = "%Y-%m-%d")

###Spread

write.csv(df_hom, "df_hom.csv")


test <- spread(df_hom_grouped,key=c(EDAD_FIXED_GROUP,indice_paz), value=n)
test

test1 <- spread(test1)







dataframe_final_stg %>%
  left_join(catalogo_estados %>% dplyr::select(indice_paz),
            by = c("ENT_REGIS" = "clave")) %>%
  mutate(matched = dplyr::match(clave, ENT_REGIS))


left_join(first_df,
          second_df %>% dplyr::select(date, elephants, cats),
          by = "date")

left_join(dataframe_final_stg,
          catalogo_estados,
          by = c("ENT_REGIS" = "clave"))







df_hom <- dataframe_final_stg

####Resultados

check <- dataframe_final %>% count(ANIO_REGIS, FILE_NAME)
check

write.csv(check, "file.csv")

dataframe_final_stg %>% distinct(ANIO_REGIS)

dataframe_final_stg %>% distinct(ANIO_OCUR)

dataframe_final_stg %>% count(ANIO_REGIS, sort = TRUE)

dataframe_final_stg %>% count(ANIO_OCUR, FILE_NAME, sort = TRUE)


dataframe_final_stg %>%
  count(entidad, sort = TRUE)







str(dataframe_final_stg)



check <- subset(dataframe_final, ANIO_OCUR == 9999)
summary(dataframe_final)

#Read dbf


defun_dataframes_new <- lapply(seq_along(defun_dataframes), function(i) {
  defun_dataframes[[i]]$FileName <- paste0("file", i)
  return(defun_dataframes[[i]])
})

my_data <- imap(defun_dataframes, ~ .x %>% mutate(file_title = .y))
