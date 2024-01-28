#install.packages("dplyr")
#install.packages("purrr")
#install.packages("foreign") 
#install.packages("tidyverse")
#install.packages("gtrendsR")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages("FactoMineR")
#install.packages("pls")
#install.packages("ggfortify")
#install.packages("factoextra")
#install.packages("tsm")
#install.packages("mFilter")
#install.packages("ggplot2")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("ggpubr")
#install.packages("rugarch")

library(rugarch)
library(ggpubr)
library(corrplot)
library(foreign)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(gtrendsR)
library(data.table)
library(vars)
library(ggcorrplot)
library(FactoMineR)
library(pls)
library(ggfortify)
library(factoextra)
library(mFilter)
library(ggplot2)
library(forecast)
library(tseries)
library(rugarch)


setwd("C:/Users/eduva/Documents/Tesis/dataset")
  
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
    dplyr::select(ENT_REGIS, MUN_REGIS, ENT_OCURR, MUN_OCURR, SEXO, MES_OCURR, ANIO_OCUR, 
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
  
dataframe_final_stg <- dplyr::select(dataframe_final_stg, -indice_paz)
  
###ENT_OCURR
  
dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_OCURR" = "clave"))
  
dataframe_final_stg <- dataframe_final_stg %>%
  rename(
    ENT_OCURR_NAME = entidad
  )
  
dataframe_final_stg <- dplyr::select(dataframe_final_stg, -indice_paz)
  
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
  
dataframe_final_stg <- dplyr::select(dataframe_final_stg, -c(entidad))
  
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
  
  
df_final <- dataframe_final_stg %>% dplyr::select(DATETIME, ENT_OCURR, SEXO,EDAD_FIXED_GROUP,indice_paz, DIM) %>%
                                 group_by(DATETIME,EDAD_FIXED_GROUP,indice_paz, SEXO, DIM) %>%
                                 summarise(REGISTERS = n())
  
df_final
  
  
###Spread de los datos del dataframe final

###Final homicidios


df_final_homicidios  <- df_final %>%
                       group_by(DATETIME) %>%
                       summarise(total_hom = sum(REGISTERS))

  
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

df_spread_sexo <- spread(df_final_sexo, key = "SEXO", value = REGISTERS)

head(df_spread_sexo)
  
###Join two tables
  
#Unimos los homicidios de edad con las categorias del indice de paz
  
df_final_hom <- df_spread_edad_fixed
  
#Unimos el previo con la tabla DIM
  
#df_final_hom <- left_join(df_final_hom, df_spread_dim)

df_final_hom <- left_join(df_final_homicidios, df_spread_sexo)  

##Dataset final
  
head(df_final_hom)


########Cargamos informacion de google Trends

###df_homicidios_csv
##id_01

###Desactivar solo cuando necesites correr de nuevo el script para obtener palabras nuevas

#homicidios_1_paper <- gtrends(keyword = c("denunciar","denuncia", "homicidio", "lesiones", "robo"), time = "all", geo="MX", Sys.sleep(20))
#df_homicidios_1_paper <- homicidios_1_paper$interest_over_time
#df_homicidios_1_paper %>% distinct(keyword)
#write.csv(df_homicidios_1_paper, "df_homicidios_paper_1.csv")

##Cargar la informacion primer grupo de variables

df_homicidios_csv <- read.csv("C:/Users/eduva/Documents/Tesis/dataset/df_homicidios_paper_1.csv")

df_homicidios_csv$date <- as.Date(df_homicidios_csv$date)

df_homicidios_csv$hits <- case_when(df_homicidios_csv$hits == "<1" ~ "0.5",
                                    .default =  df_homicidios_csv$hits)

df_homicidios_csv$hits <- as.numeric(df_homicidios_csv$hits)

df_homicidios_csv <- df_homicidios_csv %>%
                     group_by(date,keyword) %>%
                     summarise(index = mean(hits))

df_homicidios_spread_csv <- spread(df_homicidios_csv, key = keyword, value = index)

head(df_homicidios_spread_csv)
str(df_homicidios_spread_csv)

###segundo grupo de variables

#homicidios_2_paper <- gtrends(keyword = c("asalto","violación", "asesinato", "desaparecido", "desaparecida"), time = "all", geo="MX", Sys.sleep(20))
#df_homicidios_2_paper <- homicidios_2_paper$interest_over_time
#df_homicidios_2_paper %>% distinct(keyword)
#write.csv(df_homicidios_2_paper, "df_homicidios_2_paper.csv")

##Cargar la informacion segundo grupo de variables

df_homicidios_paper_2_csv <- read.csv("C:/Users/eduva/Documents/Tesis/dataset/df_homicidios_2_paper.csv")

df_homicidios_paper_2_csv$date <- as.Date(df_homicidios_paper_2_csv$date)

df_homicidios_paper_2_csv$hits <- case_when(df_homicidios_paper_2_csv$hits == "<1" ~ "0.5",
                                    .default =  df_homicidios_paper_2_csv$hits)

df_homicidios_paper_2_csv$hits <- as.numeric(df_homicidios_paper_2_csv$hits)

df_homicidios_paper_2_csv <- df_homicidios_paper_2_csv %>%
                              group_by(date,keyword) %>%
                              summarise(index = mean(hits))

df_homicidios_paper_2_spread_csv <- spread(df_homicidios_paper_2_csv, key = keyword, value = index)

head(df_homicidios_paper_2_spread_csv)
str(df_homicidios_paper_2_spread_csv)


####Tercer grupo de variables

#homicidios_3_paper <- gtrends(keyword = c("trata ","extorsión", "policía", "agresor", "militar"), time = "all", geo="MX", Sys.sleep(20))
#df_homicidios_3_paper <- homicidios_3_paper$interest_over_time
#df_homicidios_3_paper %>% distinct(keyword)
#write.csv(df_homicidios_3_paper, "df_homicidios_3_paper.csv")

##Cargar la informacion tercer grupo de variables

df_homicidios_paper_3_csv <- read.csv("C:/Users/eduva/Documents/Tesis/dataset/df_homicidios_3_paper.csv")

df_homicidios_paper_3_csv$date <- as.Date(df_homicidios_paper_3_csv$date)

df_homicidios_paper_3_csv$hits <- case_when(df_homicidios_paper_3_csv$hits == "<1" ~ "0.5",
                                            .default =  df_homicidios_paper_3_csv$hits)

df_homicidios_paper_3_csv$hits <- as.numeric(df_homicidios_paper_3_csv$hits)

df_homicidios_paper_3_csv <- df_homicidios_paper_3_csv %>%
  group_by(date,keyword) %>%
  summarise(index = mean(hits))

df_homicidios_paper_3_spread_csv <- spread(df_homicidios_paper_3_csv, key = keyword, value = index)

head(df_homicidios_paper_3_spread_csv)
str(df_homicidios_paper_3_spread_csv)

#Cuarto grupo de variables

#homicidios_4_paper <- gtrends(keyword = c("arma de fuego","pistola", "imputado", "cadáver", "feminicidio"), time = "all", geo="MX", Sys.sleep(20))
#df_homicidios_4_paper <- homicidios_4_paper$interest_over_time
#df_homicidios_4_paper %>% distinct(keyword)
#write.csv(df_homicidios_4_paper, "df_homicidios_4_paper.csv")

##Cargar la informacion tercer grupo de variables

df_homicidios_paper_4_csv <- read.csv("C:/Users/eduva/Documents/Tesis/dataset/df_homicidios_4_paper.csv")

df_homicidios_paper_4_csv$date <- as.Date(df_homicidios_paper_4_csv$date)

df_homicidios_paper_4_csv$hits <- case_when(df_homicidios_paper_4_csv$hits == "<1" ~ "0.5",
                                            .default =  df_homicidios_paper_4_csv$hits)

df_homicidios_paper_4_csv$hits <- as.numeric(df_homicidios_paper_4_csv$hits)

df_homicidios_paper_4_csv <- df_homicidios_paper_4_csv %>%
  group_by(date,keyword) %>%
  summarise(index = mean(hits))

df_homicidios_paper_4_spread_csv <- spread(df_homicidios_paper_4_csv, key = keyword, value = index)

head(df_homicidios_paper_4_spread_csv)
str(df_homicidios_paper_4_spread_csv)



####Joined data

joined_data <- df_final_hom %>% 
        left_join(df_homicidios_spread_csv, by = c("DATETIME" = "date")) %>% 
        left_join(df_homicidios_paper_2_spread_csv, by = c("DATETIME" = "date")) %>%
        left_join(df_homicidios_paper_4_spread_csv, by = c("DATETIME" = "date")) %>%
        left_join(df_homicidios_paper_3_spread_csv, by = c("DATETIME" = "date"))
  

joined_data <- filter(joined_data, DATETIME > "2003-12-01")

########Modelos

###Create PCA

pca_dataset <- dplyr::select(joined_data %>%
                             ungroup (),
                             -c(DATETIME, total_hom , Hombre, Mujer,
                             lesiones, asesinato, robo, asalto, homicidio,
                             "arma de fuego", violación, cadáver,
                             agresor, desaparecida
                             ))

#, violación, homicidio, robo
# La validacion de cifras fue exitosa, puede que no coincida por con las cifras totales por 
#loa NA
#check_data <-  dplyr::select(joined_data, DATETIME, Adulto, No_adulto, Hombre_Adulto,
#                             Hombre_No_adulto, Mujer_Adulto, Mujer_No_adulto )

#write.csv(check_data, "check_data.csv")

str(pca_dataset)

###Tenemos ceros en las correlacciones hay que quitar las variables que no nos sirven
#view()

matriz_cuant <-data.matrix(pca_dataset)

matriz_cuant

# Check for null values 

colSums(is.na(matriz_cuant))

# Estandarizar la matriz de datos


data1 <- stdize(as.matrix(matriz_cuant), center = TRUE, scale = TRUE) 
data1 <- as.data.frame(data1)
sapply(data1,mean)


boxplot(matriz_cuant, main = "Variabilidad en datos")
boxplot(data1, main = "Variabilidad en datos")


#analizar las correlaciones 

min(cor(matriz_cuant))

summary(matriz_cuant)

##modelo acp

ACP <- prcomp(matriz_cuant, center = TRUE, scale = TRUE)
plot(ACP)
ACP

ACP$rotation


#######PLOT PCA


autoplot(ACP, data = matriz_cuant)

biplot(ACP)

#prcomp$sdev -> raiz cuadrada de eigenvalores
#prcomp$rotation -> eigenvectores (coeficientes de las combinaciones lineales, que determinan a los CP)
#prcomp$x -> componentes principales

ACP#
ACP$sdev
ACP$x
acp <- as.data.frame(ACP$x)
acp
ACP$rotation


#explained variance

fviz_eig(ACP, addlabels=TRUE, hjust = -0.3)



###Preparamos el data

pca_dataset_results <- as.data.frame(ACP$x) 


data_model <- as.data.frame(cbind(joined_data$DATETIME, joined_data$total_hom, joined_data$Hombre,
                                  joined_data$Mujer, pca_dataset_results$PC1
                                  ))


colnames(data_model) <- c("datetime", "total_hom", "hombre", "mujer", "PC1")

head(data_model)

#Hay un problea con la fecha

data_model$datetime <- as.Date(data_model$datetime)

str(data_model)

#####Data model####

#convert to time series

total_hom_var <- ts(data_model$total_hom, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
hombre_hom_var <- ts(data_model$hombre, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
mujer_hom_var <- ts(data_model$mujer, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
pc1_google_var <- ts(data_model$PC1, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)


###Diferenciar las variables para que sean estacionarias

#adulto_hom_Var_log <- log(adulto_hom_var)
#ndiffs(adulto_hom_Var_log) #1 vez
#d_adulto_hom_Var_log <- diff(adulto_hom_Var_log,lag = 1)

#no_adulto_hom_var_log <- log(no_adulto_hom_var)
#ndiffs(no_adulto_hom_var_log) #1 vez
#d_no_adulto_hom_var_log <- diff(no_adulto_hom_var_log, lag = 1)

#hombre_no_adulto_hom_var_log <- log(hombre_no_adulto_hom_var)
#ndiffs(hombre_no_adulto_hom_var_log) #1 vez
#d_hombre_no_adulto_hom_var_log <-diff(hombre_no_adulto_hom_var_log, lag = 1)

#hombre_adulto_hom_var_log <- log(hombre_adulto_hom_var)
#ndiffs(hombre_adulto_hom_var_log) #1 vez
#d_hombre_adulto_hom_var_log <- diff(hombre_adulto_hom_var_log, lag = 1)

#mujer_no_adulto_hom_var_log <- log(mujer_no_adulto_hom_var)
#ndiffs(mujer_no_adulto_hom_var_log)
#d_mujer_no_adulto_hom_var_log <- diff(mujer_no_adulto_hom_var_log, lag = 1)

#mujer_adulto_hom_var_log <- log(mujer_adulto_hom_var)
#ndiffs(mujer_adulto_hom_var_log)
#d_mujer_adulto_hom_var_log <- diff(mujer_adulto_hom_var_log, lag = 1)

#la variable no se hizo log porque da nas
ndiffs(pc1_google_var)
d_pc1_google_var <- diff(pc1_google_var, lag = 1)



##Prueba de causalidad

var_model <- cbind(total_hom_var, mujer_hom_var, hombre_hom_var,  pc1_google_var)
colnames(var_model) <- c("total_hom_var", "mujer_hom_var", "hombre_hom_var", "pc1_google_var")

#var_model_hom_total <- cbind(total_hom_var, pc1_google_var)
#colnames(var_model_hom_total) <- c("total_hom_var", "pc1_google_var")

###Normality

qqnorm(var_model[,1])
qqline(var_model[,1])
plot(var_model[,1])
mean(var_model[,1])

##Primera variable

par(mfrow = c(2, 2))

qqnorm(var_model[,1], main = "Serie original")
qqline(var_model[,1])

qqnorm(log(var_model[,1]), main = "Log")
qqline(log(var_model[,1]))

qqnorm(diff(var_model[,1]),lag = 1, main = "1 dif")
qqline(diff(var_model[,1]),lag = 1)

qqnorm(diff(log(var_model[,1])),lag = 1, main = "1 dif and log")
qqline(diff(log(var_model[,1])),lag = 1)

##cuarta variable google


par(mfrow = c(2, 2))

qqnorm(var_model[,4], main = "Serie original")
qqline(var_model[,4])

qqnorm(log(var_model[,4]), main = "Log")
qqline(log(var_model[,4]))

qqnorm(diff(var_model[,4]),lag = 1, main = "1 dif")
qqline(diff(var_model[,4]),lag = 1)

qqnorm(diff(log(var_model[,4])),lag = 1, main = "1 dif and log")
qqline(diff(log(var_model[,4])),lag = 1)

#write.csv(var_model, "var_model.csv")

###Preguntar al profe prueba de granger

#Cargar archivo

#var_model <- read.csv("var_model.csv")

###PROCESO VAR

#Estacionalidad

adf_homicidios_total <- adf.test(var_model[,1])
adf_pc1_google <- adf.test(var_model[,4])
adf_pc1_google

##Diferenciamos las series

diff_homicidios <- diff(var_model[,1], lag = 1)
adf.test(diff_homicidios)
plot(diff_homicidios)

diff_mujer_homicidios <- diff(var_model[,2], lag = 1)
adf.test(diff_mujer_homicidios)
plot(diff_mujer_homicidios)

diff_hombre_homicidios <- diff(var_model[,3], lag = 1)
adf.test(diff_hombre_homicidios)
plot(diff_hombre_homicidios)

diff_pc1_google <- diff(var_model[,4], lag = 1)
adf.test(diff_pc1_google)
plot(diff_pc1_google)

var_model_diff <- cbind(diff_homicidios, diff_mujer_homicidios, diff_hombre_homicidios, diff_pc1_google )
colnames(var_model_diff) <- c("diff_homicidios", "diff_mujer_homicidios", "diff_hombre_homicidios", "diff_pc1_google")

##box cox

#var_model_lambda <- BoxCox.lambda(var_model)

#var_model_box <- BoxCox(var_model, var_model_lambda)

#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect <- VARselect(var_model_diff, lag.max = 15)
lagselect$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#    6      6      6     15 

##Estamos cumpliendo con la condicion de estabilidad, tenemos el numero correcto de regazgos

#modelo1 <- VAR(var_model[,3:7], p = 2, type = c("const"))

#14 ok

#Revisión gráfica de homicidios y google trends

#Creación de modelo con homicidios y google trends con 10 rezagos
  
estimado <- VAR(var_model_diff[,c(1,4)], p = 10, type = c("both"), exogen =NULL, lag.max = NULL,
                  season = 12)
coef(estimado)
estimado_residuals <- residuals(estimado)
summary(estimado)

par(mfrow = c(1, 1))
hist(estimado_residuals, main = "histograma residuos")

##Raices

roots(estimado, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11 <- serial.test(estimado, lags.pt =15, type = "PT.asymptotic")
ser11

#Prueba normalidad

norm1 <- normality.test(estimado, multivariate.only = TRUE)
norm1$jb.mul

##Prueba homocedasticidad varianza de los residuales

arch1 <- arch.test(estimado, lags.multi = 15)
arch1$arch.mul


qqnorm(resid(estimado))
qqline(resid(estimado))



coef(modelo1)
residuals(modelo1)

modelo1_stats <- summary(modelo1)
modelo1_stats$roots



##Prueba de autocorrelacion serial en los residuales

residual_analysis <- serial.test(modelo1, lags.pt = 15, type = "PT.asymptotic")
residual_analysis$serial


##Normalidad de los residuales

norm1 <- normality.test(modelo1, multivariate.only = TRUE)
norm1$jb.mul #los valores tienen que ser mayores que alfa


##Prueba homocedasticidad varianza de los residuales

arch1 <- arch.test(estimado, lags.multi = 15)
arch1$arch.mul











ser11 <- serial.test(modelo1, lags.pt = 16, type = "PT.asymptotic")
ser11 #p valor mayor que alfa
norm1 <- normality.test(estimado)
norm1$jb.mul #los valores tienen que ser mayores que alfa




