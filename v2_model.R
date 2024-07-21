
####

# Especificar el período de tiempo que deseas filtrar
start_date_v1 <- c(2004,1)
end_date_v1 <-   c(2020,12)

var_model_subset_v1 <- window(var_model, start = start_date_v1, end = end_date_v1)

#Lags del modelo

###Seleccionamos la variable de homicidios y el pca de google trends

lagselect_v1 <- VARselect(var_model_subset_v1[,c(2:4)], lag.max = 40)
lagselect_v1$selection

model_lags = 29
estimado_v1 <- VAR(var_model_subset_v1[,c(2:4)], p = model_lags, type = c("both"), exogen =NULL, lag.max = NULL,
                season = 12)

coef(estimado_v1)
estimado_residuals <- residuals(estimado_v1)
summary(estimado_v1)

par(mfrow = c(1, 1))
hist(estimado_residuals, main = "histograma residuos")

##Raices

roots(estimado_v1, modulus = TRUE) #raíces

#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos

ser11_v1 <- serial.test(estimado_v1, lags.pt =33, type = "PT.asymptotic")
ser11_v1

#plot(serial.test(estimado_v1))

#Prueba normalidad

norm1_v1 <- normality.test(estimado_v1, multivariate.only = TRUE)
norm1_v1$jb.mul

plot(var_model_subset_v1[,c(2:4)])

