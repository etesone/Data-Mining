source("Funciones_R.R")

# Cargar librerias
paquetes(c('readxl','fpp2','tseries','forecast','ggplot2','seasonal','descomponer','TSA'))

datos <- read_xlsx("Historical Total Students.xlsx")

head(datos)

## Create a time series object
set.seed(435)
myts <- ts(datos[,-1], start = c(2017, 365.25), frequency = 365)

head(myts)

autoplot(myts)

adf.test(myts)

ggseasonplot(myts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Cantidad de myts por mes") +  ggtitle("Seasonal plot: myts por mes")

# Descomposición de las series 
myts_comp<- decompose(myts,type=c("multiplicative"))

# Representacion de la descomposición
autoplot(myts_comp)

# Coeficientes debidos a la estacionalidad 
myts_comp$figure

# Representacion de residuos
autoplot(myts_comp$random)
mean(myts_comp$random, na.rm = T)
sd(myts_comp$random, na.rm = T)

## Contraste de normalidad de los residuos, para ver si es normal
ks.test(myts_comp$random,'pnorm') 
shapiro.test(myts_comp$random)

# Construcción del periodograma 
gperiodograma(diff(diff(myts,365),180))
gperiodograma(diff(log(myts)))

# Seleccionamos toda la serie excepto los valores del último año 
# para ajustar los modelos
myts_train<-window(myts,end=c(2019,300))

# Seleccionamos el último año para comparar predicciones 
myts_test<-window(myts,start=c(2019,301))

### Suavizado exponencial simple ses(). Predicción a un año
myts_s1=ses(myts_train, h=85)


# Distribución de residuos
print(myts_s1)
myts_s1$model
autoplot(myts_s1$residuals)

#Representamos los valores observados y los suavizados con la predicción 
autoplot(myts_s1) +
  autolayer(fitted(myts_s1), series="Fitted") +autolayer(myts_test, series="actual") +
  ylab("viajeros") + xlab("Mes/Año")


###  Suavizado Exponencial doble de Holt 
myts_sh <- holt(myts_train, h=85)

# Inspección del objeto creado y Distribución de residuos
print(myts_sh)
myts_sh$model
autoplot(myts_sh$residuals)

#Representamos los valores observados y los suavizados con la predicción 
autoplot(myts_sh) +
  autolayer(fitted(myts_sh), series="Fitted") +autolayer(myts_test, series="actual") +
  ylab("viajeros") + xlab("Mes/Año")

### Suavizado Exponencial con estacionalidad. Holt-Winters
myts_hw <- hw(myts_train, seasonal='multiplicative', h=1, level = c(80, 95))
#print(myts_hw)

myts_hw$model
autoplot(myts_hw$residuals)
checkresiduals(myts_hw)

#Representamos los valores observados y los suavizados con la predicción 
autoplot(myts_hw) +
  autolayer(fitted(myts_hw), series="Fitted") +autolayer(myts_test, series="actual") +
  ylab("viajeros") + xlab("Mes/Año")


# Se prueba la precisión de las distintas predicciones
accuracy(myts_s1,myts_test)
accuracy(myts_sh,myts_test)
accuracy(myts_hw,myts_test)

##### ARIMA

#Calculamos  las autocorrelaciones simples hasta el retardo 48
ggAcf(myts, lag=48)

# Serie diferenciada
autoplot(diff(myts,12)) +ggtitle("myts") +  xlab("A�o") +  ylab("Concentracion de myts")

# Diferenciamos uno
ggAcf(diff(myts,12), lag=48)

ggtsdisplay(diff(myts), lag.max = 48)

#Ajuste manual y visualización de residuos, primero checkeo con 3, despues con 2, queda mejor el 3 por el lag
fit1 <- myts %>%  Arima(order=c(1,1,1), seasonal=c(1,1,0)) 

fit1 %>%  residuals()  %>% ggtsdisplay()

fit2 <- myts %>%  Arima(order=c(1,1,1), seasonal=c(0,1,1)) 

fit2 %>%  residuals()  %>% ggtsdisplay()

fit3 <- myts %>%  Arima(order=c(1,1,0), seasonal=c(1,1,0)) 

fit3 %>%  residuals()  %>% ggtsdisplay()

fit4 <- myts %>%  Arima(order=c(1,1,2), seasonal=c(1,1,0)) 

fit4 %>% residuals() %>% ggtsdisplay()

#Ajuste con la función auto.arima
fit_auto <- auto.arima(myts,seasonal=TRUE)
checkresiduals(fit_auto)
fit_auto %>% residuals() %>% ggtsdisplay()

# Coeficientes de los modelos
fit1
fit2
fit3
fit4
fit_auto

#modelo fit2 y fit_auto tienen AIC mas bajo.

# Accuracy 
round(accuracy(fit2),3) # MPE mas bajo
round(accuracy(fit_auto),3)

# Predicciones
cbind("Concentracion de myts" = myts,
      "Valores ajustados" =fitted(fit2)) %>%
  autoplot() + xlab("trimestre") + ylab("") +
  ggtitle("Concentracion de myts observada y ajustada")


### Pruebas y comparaciones training/test
# Ventanas de ajuste y evaluación 
myts_tr<-window(x = myts, end=c(1986,11))
myts_tst<-window(x = myts, start=c(1986,12))


#Ajuste manual y visualización de residuos
fit2_tr <- Arima(myts_tr,order=c(1,1,1), seasonal=c(0,1,1)) 
fit_auto_tr <- Arima(myts_tr,order=c(1,0,1), seasonal=c(2,1,2)) 

# Estudio de residuos
fit_auto_tr %>% residuals() %>% ggtsdisplay()
checkresiduals(fit2_tr)
checkresiduals(fit_auto_tr)

# Valores de Ajuste en training
accuracy(fit2_tr) #MPE mas bajo
accuracy(fit_auto_tr)

## Predicciones 
pred2<-forecast(fit2_tr, h=20)
pred_auto<-forecast(fit_auto_tr, h=20)


## Valores de ajuste en test
accuracy(pred2,myts_tst)
accuracy(pred_auto,myts_tst)

# Representación conjunta un poco más visible
autoplot(pred2$mean,series='Pred2')+
  autolayer(pred_auto$mean, series='Pred_auto')+
  autolayer(myts_tst, series='Real')
