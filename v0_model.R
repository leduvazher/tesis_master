
##paqueterias


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



########Modelos

data_model_defun <- read.csv("C:/Users/eduva/Documents/Tesis/dataset/final_dataset_defun.csv")

###Create PCA

pca_dataset <- dplyr::select(data_model_defun %>%
                               ungroup (),
                             -c(DATETIME, total_hom , Hombre, Mujer,
                                lesiones, asesinato, robo, asalto, homicidio,
                                "arma.de.fuego", violación, cadáver,
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

data_model <- as.data.frame(cbind(data_model_defun$DATETIME, data_model_defun$total_hom, data_model_defun$Hombre,
                                  data_model_defun$Mujer, acp$PC1))

colnames(data_model) <- c("datetime", "total_hom", "hombre", "mujer", "PC1")

data_model <- data_model %>%
  mutate_at(vars(-datetime), as.numeric) %>%
  mutate(datetime = as.Date(datetime))

str(data_model)

write.csv(data_model, "data_model.csv", row.names = FALSE)


#####Data model####

#convert to time series

total_hom_var <- ts(data_model$total_hom, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
hombre_hom_var <- ts(data_model$hombre, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
mujer_hom_var <- ts(data_model$mujer, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
pc1_google_var <- ts(data_model$PC1, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)

var_model <- cbind(total_hom_var, mujer_hom_var, hombre_hom_var,  pc1_google_var)
colnames(var_model) <- c("total_hom_var", "mujer_hom_var", "hombre_hom_var", "pc1_google_var")
str(var_model)

plot(var_model[,1])

write.csv(var_model[,0:4], "var_model_dataset.csv")

# Especificar el período de tiempo que deseas filtrar
start_date <- c(2010,1)
end_date <-   c(2022,12)

var_model_subset <- window(var_model, start = start_date, end = end_date)

#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect <- VARselect(var_model_subset[,c(1,4)], lag.max = 35)
lagselect$selection

model_lags = 24 
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado <- VAR(var_model_subset[,c(1,4)], p = model_lags, type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)

coef(estimado)
estimado_residuals <- residuals(estimado)
summary(estimado)

par(mfrow = c(1, 1))
hist(estimado_residuals, main = "histograma residuos")

##Raices

roots(estimado, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11 <- serial.test(estimado, lags.pt =48, type = "PT.asymptotic")
ser11

plot(serial.test(estimado))

#Prueba normalidad

norm1 <- normality.test(estimado, multivariate.only = TRUE)
norm1$jb.mul



###Diferencias

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



###

# Especificar el período de tiempo que deseas filtrar
start_date_dif <- c(2007,1)
end_date_dif <-   c(2022,12)

var_model_subset_dif <- window(var_model, start = start_date_dif, end = end_date_dif)

#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect_dif <- VARselect(var_model_subset_dif[,c(1,4)], lag.max = 30)
lagselect_dif$selection

model_lags = 24
estimado_dif <- VAR(var_model_subset[,c(1,4)], p = model_lags, type = c("trend"), exogen =NULL, lag.max = NULL,
                season = 12)

coef(estimado_dif)
estimado_residuals_dif <- residuals(estimado_dif)
summary(estimado_dif)

par(mfrow = c(1, 1))
hist(estimado_residuals_dif, main = "histograma residuos")

##Raices

roots(estimado_dif, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11_dif <- serial.test(estimado_dif, lags.pt = 55, type = "PT.asymptotic")
ser11_dif

#Prueba normalidad

norm1_dif <- normality.test(estimado_dif, multivariate.only = TRUE)
norm1$jb.mul

####################################


###Seleccionamos la variable de homicidios y el pca de google trends

lagselect <- VARselect(var_model_subset[,c(1,4)], lag.max = 35)
lagselect$selection


model_lags = 24 
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado <- VAR(var_model_subset[,c(1,4)], p = max(model_lags), type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)

# Crear una matriz de 2 filas y 61 columnas con ceros
mi_matriz <- matrix(0, nrow = 2, ncol = 61)

# Columnas especificadas
column1 <- c(1, 5, 39)
column2 <- c(2, 4, 16, 23, 24)

# Agregar valores 1 a las columnas especificadas
mi_matriz[1, column1] <- 1
mi_matriz[2, column2] <- 1


var1_restrict <- restrict(estimado, method ="man", resmat = mi_matriz)
var1_restrict$varresult$pc1_google_var


summary(var1_restrict)

##Heterocedasticidad



ser11_dif <- serial.test(var1_restrict, lags.pt = 69, type = "PT.asymptotic")
ser11_dif



##Normalidad

norm1_dif <- normality.test(var1_restrict, multivariate.only = TRUE)
norm1$jb.mul

##

yf=predict(var1_restrict, n.ahead = 11, ci = 0.95, dumvar = NULL)
yf

###

fanchart(yf, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("total_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

####Modelo 2

lagselect1 <- VARselect(var_model_subset[,2:4], lag.max = 35)
lagselect1$selection

#5

model_lags1 = 30
estimado1 <- VAR(var_model_subset[,2:4], p = model_lags1, type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)

coef(estimado1)
estimado1_residuals <- residuals(estimado1)
summary_estimado1 <- summary(estimado1)
summary_estimado1

##Raices

roots(estimado1, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11_dif <- serial.test(estimado1, lags.pt = 55, type = "PT.asymptotic")
ser11_dif

#Prueba normalidad

norm1_dif_2 <- normality.test(estimado1, multivariate.only = TRUE)
norm1_dif_2$jb.mul

###Var restricted

dim(estimado1$varresult$hombre_hom_var$model)
##29 columnas

# Crear una matriz de 3 filas y 29 columnas con ceros
mi_matriz_v1 <- matrix(0, nrow = 3, ncol = 28)

# Columnas especificadas V1

column1v1 <- c(2)
column2v1 <- c(2, 5, 8)
column3v1 <- c(2, 3, 6, 13)

# Agregar valores 1 a las columnas especificadas
mi_matriz_v1[1, column1v1] <- 1
mi_matriz_v1[2, column2v1] <- 1
mi_matriz_v1[3, column3v1] <- 1

mi_matriz_v1

var1_restrictv1 <- restrict(estimado1, method ="man", resmat = mi_matriz_v1)

summary(var1_restrictv1)


#######################

###Modelo 3 - Var con google Trends desde 2004


# Especificar el período de tiempo que deseas filtrar
start_date2 <- c(2010,1)
end_date2 <-   c(2022,12)

var_model_subset_2 <- window(var_model, start = start_date2, end = end_date2)


#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect_dif_2 <- VARselect(var_model_subset_2[,c(2:4)], lag.max = 30)
lagselect_dif_2$selection

#2

model_lags = 5
estimado_2 <- VAR(var_model_subset_2[,c(2:4)], p = model_lags, type = c("both"), exogen =NULL, lag.max = NULL,
                    season = 12)

coef(estimado_2)
estimado_residuals_2 <- residuals(estimado_2)
summary(estimado_2)

par(mfrow = c(1, 1))
hist(estimado_residuals_2, main = "histograma residuos")

##Raices

roots(estimado_2, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11_dif_2 <- serial.test(estimado_2, lags.pt = 10, type = "PT.asymptotic")
ser11_dif_2

#Prueba normalidad

norm1_2 <- normality.test(estimado_2, multivariate.only = TRUE)
norm1_2$jb.mul


