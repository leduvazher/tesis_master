
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(gtrendsR)
library(data.table)
library(vars)
library(factoextra)
library(ggplot2)
library(forecast)
library(tseries)


data_model <- read.csv("~/Tesis/dataset/data_model.csv", header = TRUE)

#####Data model####

diff_pc1_google <- diff(data_model[,5], lag = 1)
adf.test(diff_pc1_google)
plot(diff_pc1_google)



#convert to time series

total_hom_var <- ts(data_model$total_hom, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
hombre_hom_var <- ts(data_model$hombre, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
mujer_hom_var <- ts(data_model$mujer, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
pc1_google_var <- ts(data_model$PC1, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)
pc1_google_dif_var <- ts(diff_pc1_google, start = c(2004,1,1), end = c(2022,12,1), frequency = 12)


####Convertimos una sola tabla


var_model <- cbind(total_hom_var, mujer_hom_var, hombre_hom_var,  pc1_google_var, pc1_google_dif_var)
colnames(var_model) <- c("total_hom_var", "mujer_hom_var", "hombre_hom_var", "pc1_google_var", "pc1_google_dif_var")
str(var_model)

plot(var_model[,1])

################################################################################
#######V0 MODEL###

# Especificar el período de tiempo que deseas filtrar
start_date <- c(2010,1)
end_date <-   c(2022,12)

var_model_subset <- window(var_model, start = start_date, end = end_date)

######MODELO


lagselect <- VARselect(var_model_subset[,c(1,4)], lag.max = 35)
lagselect$selection

model_lags = 24 
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado <- VAR(var_model_subset[,c(1,4)], p = max(model_lags), type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)



#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11 <- serial.test(estimado, lags.pt =48, type = "PT.asymptotic")
ser11

plot(serial.test(estimado))

#Prueba normalidad

norm1 <- normality.test(estimado, multivariate.only = TRUE)
norm1$jb.mul


######vAR RESTRICT

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

#Pruebas


##Heterocedasticidad

ser11 <- serial.test(var1_restrict, lags.pt = 65, type = "PT.asymptotic")
ser11

##Normalidad

norm1 <- normality.test(var1_restrict, multivariate.only = TRUE)
norm1$jb.mul


##############################################################################
##############################################################################

##NUEVOS MODELOS V1

##2004 a 2020, y dejar dos años afuera de la estimacion
##pronosticar 2 años modelos arima y dos años modelos var
##1. var con google trends
##2. var sin google trends
##3. univariados

# Especificar el período de tiempo que deseas filtrar
start_date_v1 <- c(2006,1)
end_date_v1 <-   c(2020,12)

var_model_subset_v1 <- window(var_model, start = start_date_v1, end = end_date_v1)

lagselect_v1 <- VARselect(var_model_subset_v1[,c(2:4)], lag.max = 35)
lagselect_v1$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#5     2      2      5 

model_lags_v1 = 28
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v1 <- VAR(var_model_subset_v1[,c(2:4)], p = model_lags_v1, type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)

summary(estimado_v1)
AIC(estimado_v3)
BIC(estimado_v3)

##Heterocedasticidad

ser11_dif_v1 <- serial.test(estimado_v1, lags.pt = 75, type = "PT.asymptotic")
ser11_dif_v1

##Normalidad

norm1_v1 <- normality.test(estimado_v1, multivariate.only = TRUE)
norm1_v1$jb.mul


##Forecast


yf_v1=predict(estimado_v1, n.ahead = 24, ci = 0.95, dumvar = NULL)
yf_v1

###

fanchart(yf_v1, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))




####################################V2


##2. var sin google trends

###Seleccionamos la variable de homicidios y el pca de google trends

# Especificar el período de tiempo que deseas filtrar
start_date_v2 <- c(2006,1)
end_date_v2 <-   c(2020,12)

var_model_subset_v2 <- window(var_model, start = start_date_v2, end = end_date_v2)

lagselect_v2 <- VARselect(var_model_subset_v2[,c(2,3)], lag.max = 35)
lagselect_v2$selection

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#16      2      2     16 


model_lags_v2 = 2
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v2 <- VAR(var_model_subset_v2[,c(2,3)], p = model_lags_v2, type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)


##Autocorrelación

ser11_dif_v2 <- serial.test(estimado_v2, lags.pt = 25, type = "PT.asymptotic")
ser11_dif_v2

##Normalidad

norm1_v2 <- normality.test(estimado_v2, multivariate.only = TRUE)
norm1_v2$jb.mul


###Forecast

yf_v2=predict(estimado_v2, n.ahead = 24, ci = 0.95, dumvar = NULL)
yf_v2

###

fanchart(yf_v2, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

###Modelo V3

# Especificar el período de tiempo que deseas filtrar
start_date_v3 <- c(2006,1)
end_date_v3 <-   c(2020,12)

var_model_subset_v3 <- window(var_model, start = start_date_v3, end = end_date_v3)

lagselect_v3 <- VARselect(var_model_subset_v3[,c(2,3,5)], lag.max = 50)
lagselect_v3$selection

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#2      2      1      2 

###Model

model_lags_v3 = 29
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v3 <- VAR(var_model_subset_v3[,c(2,3,5)], p = model_lags_v3, type = c("both"), exogen =NULL, lag.max = NULL,
                   season = 12)

summary(estimado_v3)

##Autocorrelacion

ser11_dif_v3 <- serial.test(estimado_v3, lags.pt = 75, type = "PT.asymptotic")
ser11_dif_v3

##Normalidad

norm1_v3 <- normality.test(estimado_v3, multivariate.only = TRUE)
norm1_v3$jb.mul


###Forecast

yf_v3=predict(estimado_v3, n.ahead = 24, ci = 0.95, dumvar = NULL)
yf_v3

###

fanchart(yf_v3, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

#write.csv(yf_v3$fcst$mujer_hom_var[,1], "forecast_muj_model_3.csv")

#### Mujeres

##Primera comparacacion Modelo v1

mujeres_test <-  window(var_model[,2], start = c(2021, 1), frequency = 12)
fit_v1 <- yf_v1$fcst$mujer_hom_var[,1]
fit_v1ts <- ts(fit_v1, start = c(2021, 1), frequency = 12)

accuracy(fit1_ts,mujeres_test)

##segunda comparacacion Modelo v2

fit_v2 <- yf_v2$fcst$mujer_hom_var[,1]
fit_v2ts <- ts(fit_v2, start = c(2021, 1), frequency = 12)

accuracy(fit_v2ts,mujeres_test)


##tercera comparacacion Modelo v3

fit_v3 <- yf_v3$fcst$mujer_hom_var[,1]
fit_v3ts <- ts(fit_v3, start = c(2021, 1), frequency = 12)

accuracy(fit_v3ts,mujeres_test)


autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Homicidios mujeres") +
  autolayer(fit_v1ts, series = "V1") +
  autolayer(fit_v2ts, series = "V2") + 
  autolayer(fit_v3ts, series = "V3")


#### Hombres

##Primera comparacacion Modelo v1

hombres_test <-  window(var_model[,3], start = c(2021, 1), frequency = 12)
fit_v1_hombres <- yf_v1$fcst$hombre_hom_var[,1]
fit_v1ts_hombres <- ts(fit_v1_hombres, start = c(2021, 1), frequency = 12)

accuracy(fit_v1ts_hombres,hombres_test)

##segunda comparacacion Modelo v2

fit_v2_hombres <- yf_v2$fcst$hombre_hom_var[,3]
fit_v2ts_hombres <- ts(fit_v2_hombres, start = c(2021, 1), frequency = 12)

accuracy(fit_v2ts_hombres,hombres_test)

##tercera comparacacion Modelo v3

fit_v3_hombres <- yf_v3$fcst$hombre_hom_var[,3]
fit_v3ts_hombres <- ts(fit_v3_hombres, start = c(2021, 1), frequency = 12)

accuracy(fit_v3ts_hombres,hombres_test)

##Grafica modelo hombres


autoplot(window(var_model[,3], start = c(2006,1), frequency = 12), ylab = "Homicidios hombres") +
  autolayer(fit_v1ts_hombres, series = "V1") +
  autolayer(fit_v2ts_hombres, series = "V2") + 
  autolayer(fit_v3ts_hombres, series = "V3")