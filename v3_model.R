
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
#plot(diff_pc1_google)



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
plot(var_model[,1], main = "Number of Homicides", ylab = "Homicides", xaxt = "n")
years <- seq(2004, 2022, by = 1)
axis(1, at = c(seq(2004, 2022, by = 1)), labels = years, las = 2)
abline(v = c(2007 + 0/12), col = "red", lwd = 2)
# Add the legend text in red
text(2007 + 1/12, max(var_model[,1], na.rm = TRUE), labels = "New security strategy", pos = 4, col = "red")


head(var_model)

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
end_date_v1 <-   c(2018,12)

var_model_subset_v1 <- window(var_model, start = start_date_v1, end = end_date_v1)

lagselect_v1 <- VARselect(var_model_subset_v1[,c(2:4)], lag.max = 35)
lagselect_v1$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#5     2      2      5 

model_lags_v1 = 30
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v1 <- VAR(var_model_subset_v1[,c(2:4)], p = model_lags_v1, type = c("both"), exogen =NULL, lag.max = NULL,
                   season = 12)

summary(estimado_v1)

estimado_v1$varresult$mujer_hom_var

##Autocorrelation

ser11_dif_v1 <- serial.test(estimado_v1, lags.pt = 85, type = "PT.asymptotic")
ser11_dif_v1

##Normality

norm1_v1 <- normality.test(estimado_v1, multivariate.only = TRUE)
norm1_v1$jb.mul

##Forecast


yf_v1=predict(estimado_v1, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v1

###

fanchart(yf_v1, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

##Restriccion

# Crear una matriz de 2 filas y 61 columnas con ceros
mi_matriz_v1 <- matrix(0, nrow = 3, ncol = 103)

# Columnas especificadas
column1_v1 <- c(2,6,33,44,45,84,89)
column2_v1 <- c(2,84)
column3_v1 <- c(70)

# Agregar valores 1 a las columnas especificadas
mi_matriz_v1[1, column1_v1] <- 1
mi_matriz_v1[2, column2_v1] <- 1
mi_matriz_v1[3, column2_v1] <- 1

var1_restrict_v1 <- restrict(estimado_v1, method ="manual", resmat = mi_matriz_v1)

summary(var1_restrict_v1)

##Autocorrelation

ser11_dif_restrict_v1 <- serial.test(var1_restrict_v1, lags.pt = 75, type = "PT.asymptotic")
ser11_dif_restrict_v1

##Normalidad

norm1_restrict_v1 <- normality.test(var1_restrict_v1, multivariate.only = TRUE)
norm1_restrict_v1$jb.mul

##Forecast

yf_v1_restricted =predict(var1_restrict_v1, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v1_restricted


fanchart(yf_v1_restricted, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))



####################################V2


##2. var sin google trends

###Seleccionamos la variable de homicidios y el pca de google trends

# Especificar el período de tiempo que deseas filtrar
start_date_v2 <- c(2006,1)
end_date_v2 <-   c(2018,12)

var_model_subset_v2 <- window(var_model, start = start_date_v2, end = end_date_v2)

lagselect_v2 <- VARselect(var_model_subset_v2[,c(2,3)], lag.max = 35)
lagselect_v2$selection

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#16      2      2     16 


model_lags_v2 = 2
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v2 <- VAR(var_model_subset_v2[,c(2,3)], p = model_lags_v2, type = c("both"), exogen =NULL, lag.max = NULL,
                   season = 12)

summary(estimado_v2)

##Autocorrelación

ser11_dif_v2 <- serial.test(estimado_v2, lags.pt = 20, type = "PT.asymptotic")
ser11_dif_v2

##Normalidad

norm1_v2 <- normality.test(estimado_v2, multivariate.only = TRUE)
norm1_v2$jb.mul

###Forecast V2

yf_v2=predict(estimado_v2, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v2

###

fanchart(yf_v2, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

###Var restricted

dim(estimado_v2$varresult$mujer_hom_var$model)
#[1] 178  18

# Crear una matriz de 2 filas y 61 columnas con ceros
mi_matriz_v2 <- matrix(0, nrow = 2, ncol = 17)

# Columnas especificadas
column1_v2 <- c(2,3)
column2_v2 <- c(2,4)

# Agregar valores 1 a las columnas especificadas
mi_matriz_v2[1, column1_v2] <- 1
mi_matriz_v2[2, column2_v2] <- 1

var1_restrict_v2 <- restrict(estimado_v2, method ="manual", resmat = mi_matriz_v2)

summary(var1_restrict_v2$varresult$mujer_hom_var$model)

summary(var1_restrict_v2)


##Autocorrelation

ser11_dif_restrict_v2 <- serial.test(var1_restrict_v2, lags.pt = 25, type = "PT.asymptotic")
ser11_dif_restrict_v2

##Normalidad

norm1_restrict_v2 <- normality.test(var1_restrict_v2, multivariate.only = TRUE)
norm1_restrict_v2$jb.mul

###Forecast V2 Restricted

yf_v2_restricted=predict(var1_restrict_v2, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v2_restricted

###

fanchart(yf_v2_restricted, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))



###Modelo V3

# Especificar el período de tiempo que deseas filtrar
start_date_v3 <- c(2006,1)
end_date_v3 <-   c(2018,12)

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

##Autocorrelation

ser11_dif_v3 <- serial.test(estimado_v3, lags.pt = 85, type = "PT.asymptotic")
ser11_dif_v3

##Normalidad

norm1_v3 <- normality.test(estimado_v3, multivariate.only = TRUE)
norm1_v3$jb.mul

###Forecast v3

yf_v3=predict(estimado_v3, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v3

###FINAL FINAL FINAL FINAL

fanchart(yf_v3, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("total_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))




###Var restricted

dim(estimado_v3$varresult$mujer_hom_var$model)
#[1] 127 101


# Crear una matriz de 2 filas y 61 columnas con ceros
mi_matriz_v3 <- matrix(0, nrow = 3, ncol = 100)

# Columnas especificadas
column1_v3 <- c(2,65,69)
column2_v3 <- c(2,74)
column3_v3 <- c(3,9,67)

# Agregar valores 1 a las columnas especificadas
mi_matriz_v3[1, column1_v3] <- 1
mi_matriz_v3[2, column2_v3] <- 1
mi_matriz_v3[3, column3_v3] <- 1

var1_restrict_v3 <- restrict(estimado_v3, method ="manual", resmat = mi_matriz_v3)

summary(var1_restrict_v3)

##Heterocedasticidad

ser11_dif_restrict_v3 <- serial.test(var1_restrict_v3, lags.pt = 60, type = "PT.asymptotic")
ser11_dif_restrict_v3

##Normalidad

norm1_restrict_v3 <- normality.test(var1_restrict_v3, multivariate.only = TRUE)
norm1_restrict_v3$jb.mul

###Forecast V3 Restricted

yf_v3_restricted=predict(var1_restrict_v3, n.ahead = 48, ci = 0.95, dumvar = NULL)
yf_v3_restricted

###

fanchart(yf_v3_restricted, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))

##Autoarima mujeres

#2. ARIMA + GT
#3. VAR
#4. VAR+GT


start_date_v4 <- c(2006,1)
end_date_v4 <-   c(2018,12)

var_model_subset_v4 <- window(var_model, start = start_date_v4, end = end_date_v4)

estimado_v4 <- auto.arima(var_model_subset_v4[,2])
estimado_v4

summary(estimado_v4)
checkresiduals(estimado_v4)

yf_v4=forecast(estimado_v4, h = 48)
autoplot(yf_v4, series = "arima")
yf_v4$mean



##Autoarima mujeres + GT

start_date_v5 <- c(2006,1)
end_date_v5 <-   c(2018,12)

var_model_subset_v5 <- window(var_model, start = start_date_v5, end = end_date_v5)

start_date_v5_1 <- c(2006,1)
end_date_v5_1 <- c(2022,12)

#future_val_gt5_subset <- window(var_model[,4], start = start_date_v5_1, end = end_date_v5_1)
future_val_gt5_subset <- var_model_subset_v5[,4]

length(var_model_subset_v5[,4])
length(var_model_subset_v5[,2])


start_date_v5_2 <- c(2019,1)
end_date_v5_2 <- c(2022,12)
forecast_gt <- window(var_model[,4], start = start_date_v5_2, end = end_date_v5_2)



future_val_gt5 <- as.matrix(future_val_gt5_subset)

length(future_val_gt5)

estimado_v5 <- auto.arima(var_model_subset_v5[,2], xreg = future_val_gt5)
estimado_v5

yf_v5 = forecast(estimado_v5, h = 48, xreg = forecast_gt)

autoplot(yf_v5)



##Autoarima hombres

#2. ARIMA + GT
#3. VAR
#4. VAR+GT


start_date_v4 <- c(2006,1)
end_date_v4 <-   c(2018,12)

var_model_subset_v4 <- window(var_model, start = start_date_v4, end = end_date_v4)

estimado_v4_hombres <- auto.arima(var_model_subset_v4[,3])
estimado_v4_hombres

summary(estimado_v4_hombres)
checkresiduals(estimado_v4_hombres)

yf_v4_hombres=forecast(estimado_v4_hombres, h = 48)
autoplot(yf_v4_hombres, series = "arima")
yf_v4_hombres$mean



##Autoarima hombres + GT

start_date_v5 <- c(2006,1)
end_date_v5 <-   c(2018,12)

var_model_subset_v5 <- window(var_model, start = start_date_v5, end = end_date_v5)

start_date_v5_1 <- c(2006,1)
end_date_v5_1 <- c(2022,12)

#future_val_gt5_subset <- window(var_model[,4], start = start_date_v5_1, end = end_date_v5_1)
future_val_gt5_subset <- var_model_subset_v5[,4]

length(var_model_subset_v5[,4])
length(var_model_subset_v5[,2])


start_date_v5_2 <- c(2019,1)
end_date_v5_2 <- c(2022,12)
forecast_gt <- window(var_model[,4], start = start_date_v5_2, end = end_date_v5_2)
future_val_gt5 <- as.matrix(future_val_gt5_subset)

length(future_val_gt5)

estimado_v5_hombres <- auto.arima(var_model_subset_v5[,3], xreg = future_val_gt5)
estimado_v5_hombres

yf_v5_hombres = forecast(estimado_v5_hombres, h = 48, xreg = forecast_gt)

autoplot(yf_v5_hombres)

#### Mujeres

##Primera comparacacion Modelo v1

mujeres_test <-  window(var_model[,2], start = c(2019, 1), frequency = 12)
fit_v1 <- yf_v1$fcst$mujer_hom_var[,1]
fit_v1ts <- ts(fit_v1, start = c(2019, 1), frequency = 12)

modelo1_accuracy_muj <- accuracy(fit_v1ts,mujeres_test)
modelo1_accuracy_muj

##segunda comparacacion Modelo v2

fit_v2 <- yf_v2$fcst$mujer_hom_var[,1]
fit_v2ts <- ts(fit_v2, start = c(2019, 1), frequency = 12)

modelo2_accuracy_muj <- accuracy(fit_v2ts,mujeres_test)
modelo2_accuracy_muj

##tercera comparacacion Modelo v3

fit_v3 <- yf_v3$fcst$mujer_hom_var[,1]
fit_v3ts <- ts(fit_v3, start = c(2019, 1), frequency = 12)

modelo3_accuracy_muj <- accuracy(fit_v3ts,mujeres_test)

##Cuarto comparacion modelo v1 restricted

fit_v1_restricted <- yf_v1_restricted$fcst$mujer_hom_var[,1]
fit_v1ts_restricted <- ts(fit_v1_restricted, start = c(2019, 1), frequency = 12)

modelo1_rest_accuracy_muj <-accuracy(fit_v1ts_restricted,mujeres_test)

##Quinta comparacion modelo v2 restricted

fit_v2_restricted <- yf_v2_restricted$fcst$mujer_hom_var[,1]
fit_v2ts_restricted <- ts(fit_v2_restricted, start = c(2019, 1), frequency = 12)

modelo2_rest_accuracy_muj <- accuracy(fit_v2ts_restricted,mujeres_test)

##Sexta comparacion modelo v3 restricted

fit_v3_restricted <- yf_v3_restricted$fcst$mujer_hom_var[,1]
fit_v3ts_restricted <- ts(fit_v3_restricted, start = c(2019, 1), frequency = 12)

modelo3_rest_accuracy_muj <- accuracy(fit_v3ts_restricted,mujeres_test)

##Septima comparacion

fit_v4 <- yf_v4$mean
fit_v4ts <- ts(fit_v4, start = c(2019, 1), frequency = 12)

modelo7_accuracy_muj <- accuracy(fit_v4ts, mujeres_test)

##Octava comparación

fit_v5 <- yf_v5$mean
fit_v5ts <- ts(fit_v5, start = c(2019, 1), frequency = 12)

modelo8_accuracy_muj <- accuracy(fit_v5ts, mujeres_test)



##Resultados mujeres

resultados_modelos_mujeres <-rbind(modelo2_accuracy_muj,
                                   modelo3_accuracy_muj, 
                                   modelo7_accuracy_muj, 
                                   modelo8_accuracy_muj)

row.names(resultados_modelos_mujeres) <- c("Model 2", "Model 3", "Model 7", "Model 8")

resultados_modelos_mujeres

write.csv(resultados_modelos_mujeres, "resultados_modelos_mujeres.csv", row.names = FALSE)

###Plot mujeres

autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Homicidios mujeres") +
  autolayer(fit_v2ts, series = "V2 model") + 
  autolayer(fit_v3ts, series = "V3 model") +
  autolayer(fit_v4ts, series = "arima") +
  autolayer(fit_v5ts, series = "arima + gt")


autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Homicidios mujeres") +
  autolayer(fit_v3ts, series = "Model")


#### Hombres

##Primera comparacacion Modelo v1

hombres_test <-  window(var_model[,3], start = c(2019, 1), frequency = 12)
fit_v1_hombres <- yf_v1$fcst$hombre_hom_var[,1]
fit_v1ts_hombres <- ts(fit_v1_hombres, start = c(2019, 1), frequency = 12)

modelo1_accuracy_hom <- accuracy(fit_v1ts_hombres,hombres_test)

##segunda comparacacion Modelo v2

fit_v2_hombres <- yf_v2$fcst$hombre_hom_var[,1]
fit_v2ts_hombres <- ts(fit_v2_hombres, start = c(2019, 1), frequency = 12)

modelo2_accuracy_hom <- accuracy(fit_v2ts_hombres,hombres_test)

##tercera comparacacion Modelo v3

fit_v3_hombres <- yf_v3$fcst$hombre_hom_var[,1]
fit_v3ts_hombres <- ts(fit_v3_hombres, start = c(2019, 1), frequency = 12)

modelo3_accuracy_hom <- accuracy(fit_v3ts_hombres, hombres_test)

##cuarta comparacion modelo v1 restricted

fit_v1_hombres_restricted <- yf_v1_restricted$fcst$hombre_hom_var[,1]
fit_v1ts_hombres_restricted <- ts(fit_v1_hombres_restricted, start = c(2019, 1), frequency = 12)

modelo1_rest_accuracy_hom <- accuracy(fit_v1ts_hombres_restricted, hombres_test)

##quinta comparacion modelo v2 restricted

fit_v2_hombres_restricted <- yf_v2_restricted$fcst$hombre_hom_var[,1]
fit_v2ts_hombres_restricted <- ts(fit_v2_hombres_restricted, start = c(2019, 1), frequency = 12)

modelo2_rest_accuracy_hom <- accuracy(fit_v2ts_hombres_restricted,hombres_test)
modelo2_rest_accuracy_hom

##sexta comparacion modelo v3 restricted


fit_v3_hombres_restricted <- yf_v3_restricted$fcst$hombre_hom_var[,1]
fit_v3ts_hombres_restricted <- ts(fit_v3_hombres_restricted, start = c(2019, 1), frequency = 12)

modelo3_rest_accuracy_hom <- accuracy(fit_v3ts_hombres_restricted,hombres_test)

##Septima comparacion

fit_v4_hombres <- yf_v4_hombres$mean
fit_v4ts_hombres <- ts(fit_v4_hombres, start = c(2019, 1), frequency = 12)

modelo7_accuracy_hom <- accuracy(fit_v4ts_hombres, hombres_test)

##Octava comparación

fit_v5_hombres <- yf_v5_hombres$mean
fit_v5ts_hombres <- ts(fit_v5_hombres, start = c(2019, 1), frequency = 12)

modelo8_accuracy_hom <- accuracy(fit_v5ts_hombres, hombres_test)



resultados_modelos_hombres <- rbind(modelo2_accuracy_hom,
                                    modelo3_accuracy_hom, 
                                    modelo7_accuracy_hom, 
                                    modelo8_accuracy_hom)

row.names(resultados_modelos_hombres) <- c("Model 2", "Model 3", "Model 7", "Model 8")

resultados_modelos_hombres

write.csv(resultados_modelos_hombres, "resultados_modelos_hombres.csv", row.names = FALSE)


##Grafica modelo hombres


autoplot(window(var_model[,3], start = c(2006,1), frequency = 12), ylab = "Homicidios hombres") +
  autolayer(fit_v1ts_hombres, series = "V1 model") +
  autolayer(fit_v2ts_hombres, series = "V2 model") + 
  autolayer(fit_v3ts_hombres, series = "V3 model") +
  autolayer(fit_v1ts_hombres_restricted, series = "V1 restricted") +
  autolayer(fit_v2ts_hombres_restricted, series = "V2 restricted") +
  autolayer(fit_v3ts_hombres_restricted, series = "V3 restricted") 


autoplot(window(var_model[,3], start = c(2006,1), frequency = 12), ylab = "Homicidios hombres") +
  autolayer(fit_v3ts_hombres, series = "Model") 




##aic, bic

AIC(estimado_v1$varresult$mujer_hom_var)
BIC(estimado_v1$varresult$mujer_hom_var)

AIC(estimado_v2$varresult$mujer_hom_var)
BIC(estimado_v2$varresult$mujer_hom_var)

AIC(estimado_v3$varresult$mujer_hom_var)
BIC(estimado_v3$varresult$mujer_hom_var)

AIC(var1_restrict_v1$varresult$mujer_hom_var)
BIC(var1_restrict_v1$varresult$mujer_hom_var)

AIC(var1_restrict_v2$varresult$mujer_hom_var)
BIC(var1_restrict_v2$varresult$mujer_hom_var)

AIC(var1_restrict_v3$varresult$mujer_hom_var)
BIC(var1_restrict_v3$varresult$mujer_hom_var)

###Hombres


AIC(estimado_v1$varresult$hombre_hom_var)
BIC(estimado_v1$varresult$hombre_hom_var)

AIC(estimado_v2$varresult$hombre_hom_var)
BIC(estimado_v2$varresult$hombre_hom_var)

AIC(estimado_v3$varresult$hombre_hom_var)
BIC(estimado_v3$varresult$hombre_hom_var)

AIC(var1_restrict_v1$varresult$hombre_hom_var)
BIC(var1_restrict_v1$varresult$hombre_hom_var)

AIC(var1_restrict_v2$varresult$hombre_hom_var)
BIC(var1_restrict_v2$varresult$hombre_hom_var)

AIC(var1_restrict_v3$varresult$hombre_hom_var)
BIC(var1_restrict_v3$varresult$hombre_hom_var)
