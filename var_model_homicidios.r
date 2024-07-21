#####Cargar archivo###

#las variables las converti en series de tiempo en un paso atras

var_model <- read.csv("location","var_model.csv")

####Lags del modelo####

###Seleccionamos la variable de homicidios y el pca de google trends para determinar el numero de rezagos
#La posición 1 corresponde al total de homicidiosy la posición 4 corresponde a la variable google trends


lagselect <- VARselect(var_model[,c(1,4)], lag.max = 15)
lagselect$selection

##Resultados
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#11      4      2     11 

###Revisión gráfica de homicidios y google trends

plot(var_model[,c(1,4)])

#Creación de modelo con google trends con 4 rezagos

estimado <- VAR(var_model[,c(1,4)],  p = 4, type = c("both"), exogen = NULL, lag.max = NULL,
               ic = c("AIC", "HQ", "SC", "FPE"))

estimado
coef(estimado)
residuals(estimado)

roots(estimado, modulus = TRUE) #raíces


#Prueba autocorrelación residuales Portmanteau Test (asymptotic) con 4 rezagos


ser11 <- serial.test(estimado, lags.pt =4, type = "PT.asymptotic")
ser11

#Prueba normalidad

norm1 <- normality.test(estimado)
norm1$jb.mul