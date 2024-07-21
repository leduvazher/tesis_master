
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

# Especificar el período de tiempo que deseas filtrar
start_date <- c(2010,1)
end_date <-   c(2022,12)

var_model_subset <- window(var_model, start = start_date, end = end_date)

#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect <- VARselect(var_model_subset[,c(1,4)], lag.max = 35)
lagselect$selection


model_lags = 24 
model_lags = c(1,2,3,6,8,12,13, 18,24)
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

plot(estimado)
