install.packages("dplyr")
install.packages("purrr")
install.packages("foreign") 
install.packages("tidyverse")
install.packages("gtrendsR")
  
  
library(foreign)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(gtrendsR)
  
  
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
                                                 if_else(dataframe_final_stg$EDAD_FIXED < 18 , "Adulto", "No_adulto" ))
  
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
###4. Estado: quitar los Estados 34 y 35 y aquellos no especificados
###5. Edad Fixed             -Filtrar registros no especificados
###6. Crear fecha con ANIO_OCURR AND MES OCURRE
  
  
  
###-Filtrar todos los homocidios menos a 1990 y lo no especificado
  
  
dataframe_final_stg_test <- filter(dataframe_final_stg, ANIO_OCUR_FIXED > 1989 & ANIO_OCUR_FIXED != "No especificado")
  
dataframe_final_stg_test %>% distinct(ANIO_OCUR_FIXED) 
  
dataframe_final_stg <- filter(dataframe_final_stg, ANIO_OCUR_FIXED > 1989 & ANIO_OCUR_FIXED != "No especificado")
  
######Filtrar mes no especificados
  
dataframe_final_stg %>% distinct(MES_OCURR)  ##Revisamos la data de los homocidios no especificados
dataframe_final_stg %>% count(MES_OCURR)
  
#Pruebas filtrar no especificados
  
dataframe_final_stg_test <- dataframe_final_stg_test %>% filter(MES_OCURR != "99")
dataframe_final_stg_test %>% distinct(MES_OCURR)
  
#Final filtrar no especificados
  
dataframe_final_stg <- dataframe_final_stg %>% filter(MES_OCURR != "99")
  
dataframe_final_stg
  
####Revisamos base de datos
  
write.csv(dataframe_final_stg, "df_hom.csv")
  
####Filtrar los Estados 34 y 35 y filtrar los no especificados
  
#Pruebas
  
dataframe_final_stg_test <- filter(dataframe_final_stg, ENT_OCURR < 33)
dataframe_final_stg_test %>% distinct(ENT_OCURR)
  
#Produccion
  
dataframe_final_stg <- filter(dataframe_final_stg, ENT_OCURR < 33)
  
#Validamos
  
write.csv(dataframe_final_stg, "check_states.csv")
  
####4. Filtrar Edad Fixed Registros no especificados
  
#Prueba
  
dataframe_final_stg_test <- filter(dataframe_final_stg, EDAD_FIXED != "No especificado")
dataframe_final_stg_test %>% distinct(EDAD_FIXED)
  
#Produccion
  
dataframe_final_stg <- filter(dataframe_final_stg, EDAD_FIXED != "No especificado")
  
#Revisar CSV
  
write.csv(dataframe_final_stg, "check_age_fixed.csv")
  
  
####5. Filtrar registros no especificados en Edad
  
#Pruebas
  
dataframe_final_stg_test <- filter(dataframe_final_stg, SEXO != "No especificado")
dataframe_final_stg_test %>% distinct(SEXO)
  
#Produccion
  
dataframe_final_stg <- filter(dataframe_final_stg, SEXO != "No especificado")
  
#Revisar CSV
  
write.csv(dataframe_final_stg, "check_sexo.csv")
  
###6. Crear fecha con ANIO_OCURR AND MES OCURRE
  
#Revisar
  
dataframe_final_stg_test <- dataframe_final_stg_test %>% 
                            mutate('DATETIME' = make_date(year = ANIO_OCUR_FIXED, month = MES_OCURR))
  
dataframe_final_stg_test %>% distinct(DATETIME)
  
#Produccion
  
dataframe_final_stg <- dataframe_final_stg %>% 
                            mutate('DATETIME' = make_date(year = ANIO_OCUR_FIXED, month = MES_OCURR))
  
write.csv(dataframe_final_stg, "check_fecha.csv")
  
  
###7. Crear variable para tomar las demas agrupaciones
  
dataframe_final_stg$DIM <- paste0(dataframe_final_stg$SEXO,"_",dataframe_final_stg$EDAD_FIXED_GROUP)
  
str(dataframe_final_stg)
  
head(dataframe_final_stg)
  
####Preparar base de datos para el modelo
####Data Normalization
  
  
df_final <- dataframe_final_stg %>% select(DATETIME, ENT_OCURR, SEXO,EDAD_FIXED_GROUP,indice_paz, DIM) %>%
                                 group_by(DATETIME,EDAD_FIXED_GROUP,indice_paz, SEXO, DIM) %>%
                                 summarise(REGISTERS = n())
  
df_final
  
  
###Spread de los datos del dataframe final
  
###Edad
  
  
df_final_edad_fixed <- df_final %>%
                       group_by(DATETIME,EDAD_FIXED_GROUP) %>%
                       summarise(REGISTERS = sum(REGISTERS))
  
df_spread_edad_fixed <- spread(df_final_edad_fixed, key = "EDAD_FIXED_GROUP", value = REGISTERS)
  
###State
  
df_final_indice_paz <- df_final %>%
                       group_by(DATETIME,indice_paz) %>%
                       summarise(REGISTERS = sum(REGISTERS))
  
df_spread_indice_paz <- spread(df_final_indice_paz, key = "indice_paz", value = REGISTERS)
  
###Hombres Mayores 18, Mujeres Mayores 18 por indice de paz
  
df_final_DIM <- df_final %>%
                       group_by(DATETIME,DIM) %>%
                       summarise(REGISTERS = sum(REGISTERS))
  
df_spread_dim <- spread(df_final_DIM, key = "DIM", value = REGISTERS)
  
##Preparar base de datos para el sexo
  
df_final_sexo <- df_final %>%
                       group_by(DATETIME,SEXO) %>%
                       summarise(REGISTERS = sum(REGISTERS))
  
  
###Join two tables
  
#Unimos los homicidios de edad con las categorias del indice de paz
  
df_final_hom <- df_spread_edad_fixed
  
#Unimos el previo con la tabla DIM
  
df_final_hom <- left_join(df_final_hom, df_spread_dim)
  
  
##Dataset final
  
head(df_final_hom)


########Cargamos informacion de google Trends

###df_homicidios_csv


df_homicidios_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_homicidios.csv")

df_homicidios_csv <- df_homicidios_csv %>%
                     group_by(date,keyword) %>%
                     summarise(index = mean(hits))

df_homicidios_spread_csv <- spread(df_homicidios_csv, key = keyword, value = index)

###df_hom_adulto

df_hom_adulto_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_adulto.csv")
head(df_hom_adulto_csv)

#Existen registros que son <1 por lo cual les estoy poniendo 0.5

df_hom_adulto_csv$hits <- case_when(df_hom_adulto_csv$hits == "<1" ~ "0.5",
                                    .default =  df_hom_adulto_csv$hits)

df_hom_adulto_csv <- df_hom_adulto_csv %>%
                     group_by(date, keyword) %>%
                     summarise(index = mean(as.numeric(hits)))

df_hom_adulto_spread_csv <- spread(df_hom_adulto_csv, key = keyword, value = index)

##df_hom_no_adulto

#Existen registros que son <1 por lo cual les estoy poniendo 0.5

df_hom_no_adulto_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_no_adulto.csv")

df_hom_no_adulto_csv %>% count(hits)

df_hom_no_adulto_csv$hits <- case_when(df_hom_no_adulto_csv$hits == "<1" ~ "0.5",
                                       .default = df_hom_no_adulto_csv$hits)

df_hom_no_adulto_csv <- df_hom_no_adulto_csv %>%
                        group_by(date, keyword) %>%
                        summarise(index = mean(as.numeric(hits)))

df_hom_no_adulto_spread_csv <- spread(df_hom_no_adulto_csv, key = keyword, value = index)

##df_hom_hombre_adulto

df_hom_hombre_adulto_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_hombre_adulto.csv")

df_hom_hombre_adulto_csv %>% count(hits)

df_hom_hombre_adulto_csv <- df_hom_hombre_adulto_csv %>%
                            group_by(date, keyword) %>%
                            summarise(index = mean(as.numeric(hits)))

df_hom_hombre_adulto_spread_csv <- spread(df_hom_hombre_adulto_csv, key = keyword, value = index)

##df_hom_ninos

df_hom_ninos_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_ninos.csv")

df_hom_ninos_csv %>% count(hits)

df_hom_ninos_csv$hits <- case_when(df_hom_ninos_csv$hits == "<1" ~ "0.5",
                                       .default = df_hom_ninos_csv$hits)

df_hom_ninos_csv  <- df_hom_ninos_csv %>%
                     group_by(date, keyword) %>%
                     summarise(index = mean(as.numeric(hits)))

df_hom_ninos_spread_csv <- spread(df_hom_ninos_csv, key = keyword, value = index)

##df_hom_muj_adulto

df_hom_muj_adulto_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_muj_adulto.csv")

df_hom_muj_adulto_csv %>% count(hits)

df_hom_muj_adulto_csv$hits <- case_when(df_hom_muj_adulto_csv$hits == "<1" ~ "0.5",
                                        .default = df_hom_muj_adulto_csv$hits)

df_hom_muj_adulto_csv  <- df_hom_muj_adulto_csv %>%
                          group_by(date, keyword) %>%
                          summarise(index = mean(as.numeric(hits)))

df_hom_muj_spread_csv <- spread(df_hom_muj_adulto_csv, key = keyword, value = index)

##df_hom_ninas

df_hom_ninas_csv <- read.csv("C:/Users/eduva/Documents/Tesis/Google Trends/queries/df_hom_ninas.csv")

df_hom_ninas_csv %>% count(hits)

df_hom_ninas_csv$hits <- case_when(df_hom_ninas_csv$hits == "<1" ~ "0.5",
                                   .default = df_hom_ninas_csv$hits)

df_hom_ninas_csv  <-      df_hom_ninas_csv %>%
                          group_by(date, keyword) %>%
                          summarise(index = mean(as.numeric(hits)))

df_hom_ninas_spread_csv <- spread(df_hom_ninas_csv, key = keyword, value = index)
