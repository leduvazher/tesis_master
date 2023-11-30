install.packages("dplyr")
install.packages("purrr")
install.packages("foreign") 


library(foreign)
library(dplyr)
library(purrr)
library(stringr)


carpeta <- "~/Documents/tesis_edu"

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

catalogo_estados <- read.csv("~/Documents/tesis_edu/catalogo_entidades.csv", header = TRUE)

dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_REGIS" = "clave"))

dataframe_final_stg <- dataframe_final_stg %>%
                                            rename(
                                            ENT_REGIS_NAME = entidad
                                            )

###ENT_OCURR

dataframe_final_stg <- left_join(dataframe_final_stg,
                                 catalogo_estados,
                                 by = c("ENT_OCURR" = "clave"))

dataframe_final_stg <- dataframe_final_stg %>%
                                            rename(
                                                    ENT_OCURR_NAME = entidad
                                                  )


dataframe_final_stg <- filter(dataframe_final_stg, PRESUNTO == 2)

filter(dataframe_final_stg, ANIO_OCUR == 99)

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
