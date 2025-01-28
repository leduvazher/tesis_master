
#install.packages("kableExtra")
#install.packages("stargazer")
#install.packages("nortest")
library(stargazer)
library(nortest)
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
library(knitr)
library(kableExtra)
library(nortest)


data_model <- read.csv("~/Tesis/dataset/data_model.csv", header = TRUE)

#####Data model####

diff_pc1_google <- diff(data_model[,5])
diff2_pc1_google <- diff(diff(data_model[,5]))
diff_muj_hom <- diff(data_model[,3])
diff_hom_hom <- diff(data_model[,4])
adf.test(diff_pc1_google)
plot(diff_pc1_google)



#convert to time series

total_hom_var <- ts(data_model$total_hom, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
hombre_hom_var <- ts(data_model$hombre, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
mujer_hom_var <- ts(data_model$mujer, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
pc1_google_var <- ts(data_model$PC1, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
pc1_google_dif_var <- ts(diff_pc1_google, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
pc1_google_2dif_var <- ts(diff2_pc1_google, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
hombre_hom_dif_var <- ts(diff_hom_hom, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)
mujer_hom_dif_var <- ts(diff_muj_hom, start = c(2004,1,1), end = c(2023,12,1), frequency = 12)


####Convertimos una sola tabla


var_model <- cbind(total_hom_var, mujer_hom_var, hombre_hom_var,
                   pc1_google_var, pc1_google_dif_var, pc1_google_2dif_var,
                   hombre_hom_dif_var,mujer_hom_dif_var)
colnames(var_model) <- c("total_hom_var", "mujer_hom_var", 
                         "hombre_hom_var", "pc1_google_var", 
                         "pc1_google_dif_var", "pc1_google_2dif_var",
                         "hombre_hom_dif_var", "mujer_hom_dif_var" )
str(var_model)
head(var_model)
var_model

plot(var_model[,1]) 
plot(var_model[,1], main = "Number of Homicides", ylab = "Homicides", xaxt = "n")
years <- seq(2004, 2023, by = 1)
axis(1, at = c(seq(2004, 2023, by = 1)), labels = years, las = 2)
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
end_date_v1 <-   c(2020,12)

var_model_subset_v1 <- window(var_model, start = start_date_v1, end = end_date_v1)

lagselect_v1 <- VARselect(var_model_subset_v1[,c(2:4)], lag.max = 35)
lagselect_v1$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#5     2      2      5 

model_lags_v1 = 35
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


yf_v1=predict(estimado_v1, n.ahead = 36, ci = 0.95, dumvar = NULL)
yf_v1

###

fanchart(yf_v1, col =c("red","red1","red2","red3","red4"), cis = NULL, names = c("mujer_hom_var"), 
         main = c("Forecast"), ylab ="var", 
         xlab = "número de observación", col.y = "red", nc=1, plot.type = c("multiple",
                                                                            "single"), mar = par("mar"), oma = par("oma"))


# Comparar con los valores reales
autoplot(window(var_model[, 3], start = c(2021, 1)), series = "Valores reales") +
  autolayer(ts(yf_v1$fcst$hombre_hom_var[, 1], start = c(2021, 1), frequency = 12), series = "Pronóstico") +
  labs(title = "Comparación de valores reales y pronóstico", y = "var", x = "número de observación") +
  theme_minimal()

autoplot(yf_v1$fcst$hombre_hom_var[, 1]) + autolayer(window(var_model[,3], start = c(2022, 1)), series = "Valores reales")



####################################V2


##2. var sin google trends

###Seleccionamos la variable de homicidios y el pca de google trends

# Especificar el período de tiempo que deseas filtrar
start_date_v2 <- c(2006,1)
end_date_v2 <-   c(2021,12)

var_model_subset_v2 <- window(var_model, start = start_date_v2, end = end_date_v2)

lagselect_v2 <- VARselect(var_model_subset_v2[,c(2,3)], lag.max = 15)
lagselect_v2$selection

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#16      2      2     16 


model_lags_v2 = 10
#model_lags = c(1,2,3,6,8,12,13, 18,24)
estimado_v2 <- VAR(var_model_subset_v2[,c(2,3)], 
                   p = model_lags_v2, 
                   type = c("both"), 
                   exogen =NULL, 
                   lag.max = NULL,
                   season = 12)

summary(estimado_v2)


##Autocorrelación

ser11_dif_v2 <- serial.test(estimado_v2, lags.pt = 15, type = "PT.asymptotic")
ser11_dif_v2

##Normalidad

norm1_v2 <- normality.test(estimado_v2, multivariate.only = TRUE)
norm1_v2$jb.mul

###Forecast V2

yf_v2=predict(estimado_v2, n.ahead = 24, ci = 0.95, dumvar = NULL)
yf_v2

yf_v2$fcst$mujer_hom_var

###


autoplot(window(var_model[, 3], start = c(2006, 1)), series = "Valores reales") +
  autolayer(ts(yf_v2$fcst$hombre_hom_var[, 1], start = c(2022, 1), frequency = 12), series = "Pronóstico") +
  labs(title = "Comparación de valores reales y pronóstico", y = "var", x = "número de observación") +
  theme_minimal()


autoplot(window(var_model[, 2], start = c(2006, 1)), series = "Valores reales") +
  autolayer(ts(yf_v2$fcst$mujer_hom_var[, 1], start = c(2022, 1), frequency = 12), series = "Pronóstico") +
  labs(title = "Comparación de valores reales y pronóstico", y = "var", x = "número de observación") +
  theme_minimal()


###Modelo V3

# Especificar el período de tiempo que deseas filtrar
#2006
start_date_v3 <- c(2006,1)
end_date_v3 <-   c(2021,12)

var_model_subset_v3 <- window(var_model, start = start_date_v3, end = end_date_v3)

lagselect_v3 <- VARselect(var_model_subset_v3[,c(2,3,6)], 
                          lag.max = 15,
                          exogen = var_model_subset_v3[,6] )
lagselect_v3$selection

#AIC(n)  HQ(n)  SC(n) FPE(n) 
#2      2      1      2 

###Model

model_lags_v3 = 10
#model_lags = c(1,2,3,6,8,12,13, 18,24)
#estimado_v3 <- VAR(var_model_subset_v3[,c(2,3,5)], p = model_lags_v3, type = c("both"), exogen =NULL, lag.max = NULL,
#                   season = 12)

estimado_v3 <- VAR(var_model_subset_v3[,c(2,3)], 
                   p = model_lags_v3, 
                   type = c("both"),
                   exogen = var_model_subset_v3[,6],
                   season = 36)
summary(estimado_v3)

##Autocorrelation
#85
ser11_dif_v3 <- serial.test(estimado_v3, lags.pt = 20, type = "PT.asymptotic")
ser11_dif_v3

##Normalidad

norm1_v3 <- normality.test(estimado_v3, multivariate.only = TRUE)
norm1_v3$jb.mul


future_exog <- window(var_model[, 6], start = c(2022, 1), end = c(2023, 12)) 
future_exog <- as.matrix(future_exog)


###Forecast v3

dim(future_exog)

yf_v3=predict(estimado_v3, 
              n.ahead = 24, 
              dumvar = future_exog)
yf_v3$fcst

###FINAL FINAL FINAL FINAL

autoplot(window(var_model[, 3], start = c(2006, 1)), series = "Valores reales") +
  autolayer(ts(yf_v3$fcst$hombre_hom_var[, 1], start = c(2022, 1), frequency = 12), series = "Pronóstico") +
  labs(title = "Comparación de valores reales y pronóstico", y = "var", x = "número de observación") +
  theme_minimal()


autoplot(window(var_model[, 2], start = c(2006, 1)), series = "Valores reales") +
  autolayer(ts(yf_v3$fcst$mujer_hom_var[, 1], start = c(2022, 1), frequency = 12), series = "Pronóstico") +
  labs(title = "Comparación de valores reales y pronóstico", y = "var", x = "número de observación") +
  theme_minimal()




##Autoarima mujeres

#2. ARIMA + GT
#3. VAR
#4. VAR+GT


start_date_v4 <- c(2006,1)
end_date_v4 <-   c(2021,12)

var_model_subset_v4 <- window(var_model, start = start_date_v4, end = end_date_v4)

#estimado_v4 <- auto.arima(var_model_subset_v4[,2],
#                          max.p = 1,        # Rango para el parámetro p
#                          max.d = 0,        # Rango para el parámetro d
#                          max.q = 0,        # Rango para el parámetro q
#                          seasonal = TRUE,  # Considerar componentes estacionales
#                          max.P = 3,        # Rango para el parámetro estacional P
#                          max.D = 1,        # Rango para el parámetro estacional D
#                          max.Q = 0,        # Rango para el parámetro estacional Q
#                          stepwise = FALSE, # Desactivar la selección paso a paso
#                          trace = TRUE,
#                          stationary = FALSE,
#                          nmodels = 200,
#                          allowdrift = TRUE)

estimado_v4 <- arima(var_model_subset_v4[,2],
                     order = c(0, 1, 1), 
                     seasonal = c(1,0,1))

 summary(estimado_v4)
checkresiduals(estimado_v4)

yf_v4=forecast(estimado_v4, h = 24)
autoplot(yf_v4, series = "arima")
yf_v4$mean

jarque_bera_test <- jarque.bera.test(residuals(estimado_v4))
print(jarque_bera_test)

shapiro_test <- shapiro.test(residuals(estimado_v4))
shapiro_test

ad_test_arima_muj <- ad.test(residuals(estimado_v4))
print(ad_test_arima_muj)

autoplot(yf_v4) + autolayer(window(var_model[,2], start = c(2022, 1)), series = "Valores reales")



##ARIMA mujeres + GT

start_date_v5 <- c(2006,1)
end_date_v5 <-   c(2021,12)

var_model_subset_v5 <- window(var_model, start = start_date_v5, end = end_date_v5)

#future_val_gt5_subset <- window(var_model[,4], start = start_date_v5_1, end = end_date_v5_1)
future_val_gt5_subset <- window(var_model[,6], start = start_date_v5, end = end_date_v5)

# Asegurarse de que las longitudes coincidan
length(var_model_subset_v5[,3])  # Serie de tiempo de hombres
length(future_val_gt5_subset)

# Ajustar el modelo ARIMA con la variable exógena
#estimado_v5_mujeres <- auto.arima(var_model_subset_v5[,2], 
#                                  xreg = future_val_gt5_subset, 
#                                  max.p = 2,        # Rango para el parámetro p
#                                  max.d = 2,        # Rango para el parámetro d
#                                  max.q = 1,        # Rango para el parámetro q
#                                  seasonal = FALSE,  # Considerar componentes estacionales
#                                  max.P = 2,        # Rango para el parámetro estacional P
#                                  max.D = 2,        # Rango para el parámetro estacional D
#                                  max.Q = 0,        # Rango para el parámetro estacional Q
#                                  stepwise = FALSE, # Desactivar la selección paso a paso
#                                  trace = TRUE,
#                                  stationary = FALSE,
#                                  nmodels = 200)

gt = as.matrix(var_model_subset_v5[, 6])
dim(gt)
estimado_v5_mujeres <- Arima(var_model_subset_v5[,2],
                             order = c(1, 0, 1), 
                             seasonal = c(0,0,0),
                             xreg =gt)

summary(estimado_v5_mujeres)

# Verificar los residuos del modelo
checkresiduals(estimado_v5_mujeres)

# Pronosticar para el período 2022-2023
start_date_v5_2 <- c(2022, 1)
end_date_v5_2 <- c(2023, 12)
forecast_gt <- window(var_model[,6], start = start_date_v5_2, end = end_date_v5_2)
forecast_gt <- as.matrix(forecast_gt)
dim(forecast_gt)

# Realizar el pronóstico
yf_v5_mujeres <- forecast(estimado_v5_mujeres, 
                          h = 24, 
                          level = 95,
                          xreg = forecast_gt)  # h = 24 para 2 años (2022-2023)

# Graficar el pronóstico
autoplot(yf_v5_mujeres) + autolayer(window(var_model[,2], start = c(2022, 1)), series = "Valores reales")

# Prueba de ADF para los residuos del modelo
ad_test_arima_muj_gt <- ad.test(residuals(yf_v5_mujeres))
print(ad_test_arima_muj_gt)


#####Autoarima hombres

#2. ARIMA + GT
#3. VAR
#4. VAR+GT


start_date_v4 <- c(2006,1)
end_date_v4 <-   c(2021,12)

var_model_subset_v4 <- window(var_model, start = start_date_v4, end = end_date_v4)

estimado_v4_hombres <- arima(var_model_subset_v4[,3], 
                             order = c(2, 0, 0), 
                             seasonal = c(1,0,0))
estimado_v4_hombres

summary(estimado_v4_hombres)
checkresiduals(estimado_v4_hombres)

yf_v4_hombres=forecast(estimado_v4_hombres, h = 24)
autoplot(yf_v4_hombres, series = "arima")
yf_v4_hombres$mean

autoplot(yf_v4_hombres) + autolayer(window(var_model[,3], start = c(2022, 1)), series = "Valores reales")

ad_test_arima_hom <- ad.test(residuals(estimado_v4_hombres))
print(ad_test_arima_hom)



##Autoarima hombres + GT

start_date_v5 <- c(2006,1)
end_date_v5 <-   c(2021,12)

var_model_subset_v5 <- window(var_model, start = start_date_v5, end = end_date_v5)

gt = as.matrix(var_model_subset_v5[, 6])

estimado_v5_hombres <- Arima(var_model_subset_v5[, 3],
                             order = c(0, 2, 2), 
                             seasonal = c(0, 0, 0),
                             xreg = gt)

summary(estimado_v5_hombres)
checkresiduals(estimado_v5_hombres)



autoplot(estimado_v5_hombres)

# Pronosticar para el período 2022-2023
start_date_v5_2 <- c(2022, 1)
end_date_v5_2 <- c(2023, 12)
forecast_gt <- window(var_model[,6], start = start_date_v5_2, end = end_date_v5_2)
forecast_gt <- as.matrix(forecast_gt)
dim(forecast_gt)


yf_v5_hombres = forecast(estimado_v5_hombres, 
                         h = 24,
                         xreg = forecast_gt)
yf_v5_hombres$mean

autoplot(yf_v5_hombres) + 
  autolayer(window(var_model[,3], start = c(2022, 1)), series = "Valores reales")

ad_test_arima_hom_gt <- ad.test(residuals(estimado_v5_hombres))
print(ad_test_arima_hom_gt)

#### Mujeres

##segunda comparacacion Modelo v2 VAR sin Google Trends

fit_v2 <- yf_v2$fcst$mujer_hom_var[,1]
fit_v2ts <- ts(fit_v2, start = c(2022, 1), frequency = 12)


modelo2_accuracy_muj <- accuracy(fit_v2ts,mujeres_test)
modelo2_accuracy_muj

##tercera comparacacion Modelo v3 VAR con Google Trends

fit_v3 <- yf_v3$fcst$mujer_hom_var[,1]
fit_v3ts <- ts(fit_v3, start = c(2022, 1), frequency = 12)
modelo3_accuracy_muj <- accuracy(fit_v3ts,mujeres_test)


##comparacion - Modelo ARIMA sin GT

yf_v4$x
yf_v4$mean
fit_v4 <- yf_v4$mean
fit_v4ts <- ts(fit_v4, start = c(2022, 1), frequency = 12)
fit_v4ts

modelo7_accuracy_muj <- accuracy(fit_v4ts, mujeres_test)

##Modelo ARIMA + GT

fit_v5 <- yf_v5_mujeres$mean
fit_v5ts <- ts(fit_v5, start = c(2022, 1), frequency = 12)
fit_v5ts

modelo8_accuracy_muj <- accuracy(fit_v5ts, mujeres_test)

##Resultados mujeres

resultados_modelos_mujeres <-rbind(modelo2_accuracy_muj,
                                   modelo3_accuracy_muj, 
                                   modelo7_accuracy_muj, 
                                   modelo8_accuracy_muj)

row.names(resultados_modelos_mujeres) <- c("VAR", "VAR + GT", "ARIMA", "ARIMA + GT")

resultados_modelos_mujeres

write.csv(resultados_modelos_mujeres, "resultados_modelos_mujeres.csv", row.names = TRUE)

###Plot mujeres

autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Female Homicides") +
  autolayer(fit_v2ts, series = "VAR") + 
  autolayer(fit_v3ts, series = "VAR + GT") +
  autolayer(fit_v4ts, series = "ARIMA") +
  autolayer(fit_v5ts, series = "ARIMA + GT")


autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Homicidios mujeres") +
  autolayer(fit_v2ts, series = "ARIMA") + 
  autolayer(fit_v3ts, series = "ARIMA + GT") +
  autolayer(fit_v4ts, series = "arima") 



autoplot(window(var_model[,2], start = c(2006,1), frequency = 12), ylab = "Homicidios mujeres") +
  autolayer(fit_v5ts, series = "Model")


#### Hombres

##Comparación VAR sin GT

hombres_test <-  window(var_model[,3], start = c(2022, 1), frequency = 12)


yf_v2$fcst

fit_v2_hombres <- yf_v2$fcst$hombre_hom_var[,1]
fit_v2ts_hombres <- ts(fit_v2_hombres, start = c(2022, 1), frequency = 12)

modelo2_accuracy_hom <- accuracy(fit_v2ts_hombres,hombres_test)

##Comparacion VAR + GT

fit_v3_hombres <- yf_v3$fcst$hombre_hom_var[,1]
fit_v3ts_hombres <- ts(fit_v3_hombres, start = c(2022, 1), frequency = 12)

modelo3_accuracy_hom <- accuracy(fit_v3ts_hombres, hombres_test)

##Comparacion ARIMA SIN GT

fit_v4_hombres <- yf_v4_hombres$mean
fit_v4ts_hombres <- ts(fit_v4_hombres, start = c(2022, 1), frequency = 12)

modelo7_accuracy_hom <- accuracy(fit_v4ts_hombres, hombres_test)

##Comparacion ARIMA MAS GT

fit_v5_hombres <- yf_v5_hombres$mean
fit_v5ts_hombres <- ts(fit_v5_hombres, start = c(2022, 1), frequency = 12)

modelo8_accuracy_hom <- accuracy(fit_v5ts_hombres, hombres_test)



resultados_modelos_hombres <- rbind(modelo2_accuracy_hom,
                                    modelo3_accuracy_hom, 
                                    modelo7_accuracy_hom, 
                                    modelo8_accuracy_hom)

row.names(resultados_modelos_hombres) <- c("VAR", "VAR + GT", "ARIMA", "ARIMA + GT")

resultados_modelos_hombres

write.csv(resultados_modelos_hombres, "resultados_modelos_hombres.csv", row.names = TRUE)


##Grafica modelo hombres


autoplot(window(var_model[,3], start = c(2006,1), frequency = 12), ylab = "Male Homicides") +
  autolayer(fit_v2ts_hombres, series = "VAR") + 
  autolayer(fit_v3ts_hombres, series = "VAR + GT") +
  autolayer(fit_v4ts_hombres, series = "ARIMA") +
  autolayer(fit_v5ts_hombres, series = "ARIMA + GT")  


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
