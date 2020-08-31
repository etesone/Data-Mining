setwd('C:/Users/etesone/Desktop/MASTER UCM/Mineria de Datos y Modelizacion Predictiva/Evaluacion 1')

source("Funciones_R.R")

paquetes(c("questionr","psych","car","corrplot","readxl","ggplot2","gdata","caret","lmSupport","rpart","glmnet","dplyr","epiDisplay","pROC","rpart.plot"))

datos_EE <- read_excel("DatosEleccionesEspana.xlsx")

str(datos_EE)

summary(datos_EE)

datos_EE$NameConcat<- paste(datos_EE$Name, ', ', datos_EE$CCAA)# hay valores duplicados que evitan que pueda hacer ciertas cosas. por eso concatene

datos_EE <- subset(datos_EE, select=c(NameConcat,Name:Explotaciones))

datos_EE<- as.data.frame(datos_EE[,-c(2,7:8,10:12)])

datos_EE[,c(1,3,7,29,33)] <- lapply(datos_EE[,c(1,3,7,29,33)], factor) # convierto la variable NameConcat a factor para luego convertirlo a nombres de fila

sapply(Filter(is.numeric, datos_EE),function(x) length(unique(x)))

dfplot(datos_EE)

###### Limpieza (Obligatorio)

datos_EE$Densidad<-recode.na(datos_EE$Densidad,"?") #cambio ? por NA

datos_EE$CCAA<-car::recode(datos_EE$CCAA, 
                           "c('Ceuta','Melilla','Baleares')='Baleares, Ceuta y Melilla'") #agrupo baleares, ceuta y melilla

datos_EE$ForeignersPtge<-replace(datos_EE$ForeignersPtge, which((datos_EE$ForeignersPtge < 0)), NA) # cambio valores fuera de rango a NA

###### Transformacion de algunas variables a binarias (Obligatorio)

datos_EE$ActividadEconomica<-car::recode(datos_EE$ActividadPpal, 
                                         "c('Construccion','Industria','Servicios','ComercTTEHosteleria')='Sectores empresariales'") #agrupo C, C, I y S

datos_EE$Industria_bin<-factor(replace(datos_EE$Industria, which(datos_EE$Industria > 0), 1))
freq(datos_EE$Industria_bin)

datos_EE$Construccion_bin<-factor(replace(datos_EE$Construccion, which(datos_EE$Construccion > 0), 1))
freq(datos_EE$Construccion_bin)

datos_EE$ComercTTEHosteleria_bin<-factor(replace(datos_EE$ComercTTEHosteleria, which(datos_EE$ComercTTEHosteleria > 0), 1))
freq(datos_EE$ComercTTEHosteleria_bin)

datos_EE$Servicios_bin<-factor(replace(datos_EE$Servicios, which(datos_EE$Servicios > 0), 1))
freq(datos_EE$Servicios_bin)

varObjCont<-datos_EE$Izda_Pct
varObjBin<-datos_EE$Derecha

###### Categorizacion de variables continuas no normales (opcional, comparar contra las mismas variables atipicas a NA)

datos_EE$Explotaciones<-ifelse(datos_EE$Explotaciones< 78.5, 0, 1)

datos_EE$SUPERFICIE<-ifelse(datos_EE$SUPERFICIE< 3339.706, 0, ifelse(datos_EE$SUPERFICIE>=3339.706 & datos_EE$SUPERFICIE <10959.13, 1, ifelse(datos_EE$SUPERFICIE>=10959.13,2, NA)))

datos_EE$Pob2010<-ifelse(datos_EE$Pob2010< 269.5, 0, 1)

datos_EE$inmuebles<-ifelse(datos_EE$inmuebles< 293.5, 0, 1)

datos_EE$Servicios<-ifelse(datos_EE$Servicios< 2, 0, ifelse(datos_EE$Servicios>=2 & datos_EE$Servicios <34.5, 1, ifelse(datos_EE$Servicios>=34.5, 2, NA)))

datos_EE$ComercTTEHosteleria<-ifelse(datos_EE$ComercTTEHosteleria< 12.5, 0, 1)

datos_EE$Construccion<-ifelse(datos_EE$Construccion< 2, 0, ifelse(datos_EE$Construccion>=2 & datos_EE$Construccion <17.5, 1, ifelse(datos_EE$Construccion>=17.5, 2, NA)))

datos_EE$ConstructionUnemploymentPtge<-ifelse(datos_EE$ConstructionUnemploymentPtge< 0.821, 0, 1)


datos_EE$IndustryUnemploymentPtge<-ifelse(datos_EE$IndustryUnemploymentPtge< 0.338, 0, 
                                          ifelse(datos_EE$IndustryUnemploymentPtge>=0.338 & datos_EE$IndustryUnemploymentPtge <4.9945, 1, 
                                                 ifelse(datos_EE$IndustryUnemploymentPtge>=4.9945 & datos_EE$IndustryUnemploymentPtge<11.5695, 2, 
                                                        ifelse(datos_EE$IndustryUnemploymentPtge>=11.5695, 3,NA))))

datos_EE$AgricultureUnemploymentPtge<-ifelse(datos_EE$AgricultureUnemploymentPtge< 5.757, 0, 1)

datos_EE$UnemployLess25_Ptge<-ifelse(datos_EE$UnemployLess25_Ptge< 7.6465, 0, 1)

datos_EE$DifComAutonPtge<-ifelse(datos_EE$DifComAutonPtge< 26.169, 0, 1)

datos_EE$SameComAutonDiffProvPtge<-ifelse(datos_EE$SameComAutonDiffProvPtge< 9.5105, 0, 1)

datos_EE$ForeignersPtge<-ifelse(datos_EE$ForeignersPtge< 0.035, 0, ifelse(datos_EE$ForeignersPtge>=0.035 & datos_EE$ForeignersPtge <3.675, 1, 
                                                                          ifelse(datos_EE$ForeignersPtge>=3.675, 2, NA)))

datos_EE$TotalCensus<-ifelse(datos_EE$TotalCensus< 241.5, 0, 1)

datos_EE$Population<-ifelse(datos_EE$Population< 262.5, 0, 1)

datos_EE$Industria<-ifelse(datos_EE$Industria< 34.20469, 0, 1)

datos_EE$totalEmpresas<-ifelse(datos_EE$totalEmpresas< 34.3927, 0, 1)

datos_EE[,c(4:5,13,15:17,20:22,24:28,30:32,36)] <- lapply(datos_EE[,c(4:5,13,15:17,20:22,24:28,30:32,36)], factor)

######### cambio de atipicos a NA

input_EE<- as.data.frame(datos_EE[,-c(1,6:7)])
row.names(input_EE)<-datos_EE$NameConcat 

str(input_EE)

psych::describe(Filter(is.numeric, input_EE))

sapply(Filter(is.numeric, input_EE),function(x) atipicosAmissing(x)[[2]])/nrow(input_EE)

input_EE[,as.vector(which(sapply(input_EE, class)=="numeric"))]<-sapply(Filter(is.numeric, input_EE),function(x) atipicosAmissing(x)[[1]])

sum(is.na(input_EE))  

summary(input_EE)

dfplot(input_EE)

#Es evidente que la presencia de perdidos en totalEmpresas presenta correlación con la presencia de perdidos en otras variables como pob2010, comercio y pobchange. 
#Interpretación, los municipios que tienen perdida la variable empresas también tienen perdidas con bastante probabilidad las otras variables. 
#Esto puede ser lógico pues los datos se recogen por municipio... 
corrplot(cor(is.na(input_EE[colnames(input_EE)[colSums(is.na(input_EE))>0]])),method = "ellipse",type = "upper")

input_EE$prop_missings<-apply(is.na(input_EE),1,mean)
summary(input_EE$prop_missings)
(prop_missingsVars<-apply(is.na(input_EE),2,mean))

######Transformacion de NA a aleatorios

input_EE[,as.vector(which(sapply(input_EE, class)=="numeric"))]<-sapply(Filter(is.numeric, input_EE),function(x) ImputacionCuant(x,"aleatorio"))

input_EE[,as.vector(which(sapply(input_EE, class)=="numeric"))]<-sapply(Filter(is.numeric, input_EE),function(x) ImputacionCuant(x,"aleatorio"))

input_EE[,as.vector(which(sapply(input_EE, class)=="numeric"))]<-sapply(Filter(is.numeric, input_EE),function(x) ImputacionCuant(x,"aleatorio"))

input_EE[,as.vector(which(sapply(input_EE, class)=="factor"))]<-sapply(Filter(is.factor, input_EE),function(x) ImputacionCuali(x,"aleatorio"))

input_EE[,as.vector(which(sapply(input_EE, class)=="character"))] <- lapply(input_EE[,as.vector(which(sapply(input_EE, class)=="character"))] , factor)

summary(input_EE)

saveRDS(cbind(varObjBin,varObjCont,input_EE),"EleccionesEspanaDep")

datos_EED<-cbind(varObjBin,varObjCont,input_EE)

str(datos_EED)

sapply(Filter(is.numeric, datos_EED),function(x) length(unique(x)))

dfplot(datos_EED)

input_EED<-datos_EED[,-(1:2)]

input_EED$aleatorio<-runif(nrow(input_EED))
input_EED$aleatorio2<-runif(nrow(input_EED))

graficoVcramer(input_EED,varObjBin)

##### Transformacion de variables numericas

input_cont<-cbind(input_EED,Transf_Auto(Filter(is.numeric, input_EED),varObjCont))

sapply(Filter(is.numeric, input_EED)[,-ncol(Filter(is.numeric, input_EED))],function(x) length(unique(x)))

input_bin<-cbind(input_EED,Transf_Auto(Filter(is.numeric, input_EED)[,-44],varObjBin))

saveRDS(data.frame(input_bin,varObjBin),"todo_bin_V")

saveRDS(data.frame(input_cont,varObjCont),"todo_cont_v")

##### Variable Objetivo Binaria

todo_bin<-data.frame(input_EED,varObjBin)

graficoVcramer(input_EED,varObjBin)

#Veo gráficamente el efecto de dos variables cualitativas sobre la binaria
str(input_EED)
mosaico_targetbinaria(input_EED$ActividadEconomica,varObjBin,"Sectores de comercio, servicio, industria y construccion") #Municipios con Otros sectores tienden a votar mayoria Derecha
mosaico_targetbinaria(input_EED$ActividadPpal,varObjBin,"Clasificacion") #esta sí influye
mosaico_targetbinaria(input_EED$SUPERFICIE,varObjBin,"Clasificacion") #no influye
mosaico_targetbinaria(input_EED$Densidad,varObjBin,"Clasificacion") #Municipios con muy baja densidad tienden a votar mayoritariamente Derecha
mosaico_targetbinaria(input_EED$Servicios,varObjBin,"Clasificacion") #no influye

#Veo gráficamente el efecto de dos variables cuantitativas sobre la binaria
boxplot_targetbinaria(input_EED$Age_over65_pct,varObjBin,"% de mayores de 65") # municipios con mayor porcentaje tienden a votar derecha mas que izquierda
boxplot_targetbinaria(input_EED$PersonasInmueble,varObjBin,"Personas por inmueble") # mientras mas personas por inmueble en un municipio, menos votan mayoria derecha.

hist_targetbinaria(input_EED$Age_over65_pct,varObjBin,"% mayores de 65") 
hist_targetbinaria(input_EED$PersonasInmueble,varObjBin,"Personas por inmueble")

#Hago la partición
set.seed(536756)
trainIndex3 <- createDataPartition(todo_bin$varObjBin, p=0.8, list=FALSE)
data_train_bin <- todo_bin[trainIndex3,]
data_test_bin <- todo_bin[-trainIndex3,]

#pruebo un primer modelo sin las transformadas
modelo1b<-glm(varObjBin~.,data=data_train_bin,family=binomial)
summary(modelo1b)
pseudoR2(modelo1b,data_train_bin,"varObjBin")
pseudoR2(modelo1b,data_test_bin,"varObjBin")
modelo1b$rank #número de parámetros

#curvaRoc<-roc(data_test_bin$varObjBin, predict(modelo1b,data_test_bin,type = "response"), direction="<")
#plot(curvaRoc)
#fijandome en la importancia de las variables, selecciono aquellas por encima de las aleatorias
modelo2b<-glm(varObjBin~CCAA+UnemployLess25_Ptge+ConstructionUnemploymentPtge+ServicesUnemploymentPtge+IndustryUnemploymentPtge+AgricultureUnemploymentPtge+
                Unemploy25_40_Ptge+ActividadPpal+DifComAutonPtge+Age_19_65_pct+ForeignersPtge+Industria,data=data_train_bin,family=binomial)
summary(modelo2b)
pseudoR2(modelo2b,data_train_bin,"varObjBin")
pseudoR2(modelo2b,data_test_bin,"varObjBin")
modelo2b$rank

#
modelo3b<-glm(varObjBin~CCAA+ConstructionUnemploymentPtge+AgricultureUnemploymentPtge+ActividadPpal+DifComAutonPtge+Age_19_65_pct+
                ForeignersPtge+Industria+Construccion+Servicios+ComercTTEHosteleria,data=data_train_bin,family=binomial)
summary(modelo3b)
pseudoR2(modelo3b,data_train_bin,"varObjBin")
pseudoR2(modelo3b,data_test_bin,"varObjBin")
modelo3b$rank

#
modelo4b<-glm(varObjBin~CCAA+ConstructionUnemploymentPtge+AgricultureUnemploymentPtge+ActividadPpal+DifComAutonPtge+Age_19_65_pct+
                ForeignersPtge+Industria+CCAA:Industria+Age_over65_pct:Age_0.4_Ptge,data=data_train_bin,family=binomial)
summary(modelo4b)
pseudoR2(modelo4b,data_train_bin,"varObjBin")
pseudoR2(modelo4b,data_test_bin,"varObjBin")
modelo4b$rank

#
modelo5b<-glm(varObjBin~CCAA+ConstructionUnemploymentPtge+AgricultureUnemploymentPtge+ActividadPpal+DifComAutonPtge+Age_19_65_pct+
                ForeignersPtge+Industria+CCAA:Industria+Age_over65_pct+Age_0.4_Ptge,data=data_train_bin,family=binomial)
summary(modelo5b)
pseudoR2(modelo5b,data_train_bin,"varObjBin")
pseudoR2(modelo5b,data_test_bin,"varObjBin")
modelo5b$rank

#
modelo6b<-glm(varObjBin~CCAA+ConstructionUnemploymentPtge:AgricultureUnemploymentPtge+ActividadEconomica+DifComAutonPtge+
                ForeignersPtge+Industria+CCAA:Industria+Age_over65_pct:Age_0.4_Ptge,data=data_train_bin,family=binomial)
summary(modelo6b)
pseudoR2(modelo6b,data_train_bin,"varObjBin")
pseudoR2(modelo6b,data_test_bin,"varObjBin")
modelo6b$rank

#Validacion cruzada repetida para elegir entre todos
auxVarObj<-todo_bin$varObjBin
todo_bin$varObjBin<-make.names(todo_bin$varObjBin) #formateo la variable objetivo para que funcione el codigo
total<-c()
modelos<-sapply(list(modelo1b,modelo2b,modelo3b,modelo4b,modelo5b,modelo6b),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = todo_bin,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,data.frame(roc=vcr$resample[,1],modelo=rep(paste("Modelo",i),
                                                                nrow(vcr$resample))))
}
boxplot(roc~modelo,data=total,main="Área bajo la curva ROC") #el 6 es peor, los otros parecidos
aggregate(roc~modelo, data = total, mean) # probablemente el 4 sea el mejor
aggregate(roc~modelo, data = total, sd)

#recupero la variable objetivo en su formato
todo_bin$varObjBin<-auxVarObj

#miro el numero de parametros
modeloInicial$rank
modelo2b$rank 
modelo3b$rank#tiene la menor cantidad de parametros lejos y funciona practicamente igual de bien
modelo4b$rank #tiene muchos parametros
modelo5b$rank

## BUscamos el mejor punto de corte

#gráfico de las probabilidades obtenidas
hist_targetbinaria(predict(modelo3b, newdata=data_test_bin,type="response"),data_test_bin$varObjBin,"probabilidad")

#probamos dos
sensEspCorte(modelo3b,data_test_bin,"varObjBin",0.5,"1")
sensEspCorte(modelo3b,data_test_bin,"varObjBin",0.75,"1")

## generamos una rejilla de puntos de corte
posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo3b,data_test_bin,"varObjBin",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#Los comparamos
sensEspCorte(modelo3b,data_test_bin,"varObjBin",0.47,"1")
sensEspCorte(modelo3b,data_test_bin,"varObjBin",0.45,"1")

# Vemos las variables más importantes del modelo ganador
impVariablesLog(modelo3b,"varObjBin",dd=data_test_bin) 

# Vemos los coeficientes del modelo ganador
coef(modelo3b)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo3b,data_train_bin,"varObjBin")
pseudoR2(modelo3b,data_test_bin,"varObjBin")
roc(data_train_bin$varObjBin, predict(modelo3b,data_train_bin,type = "response"), direction="<")
roc(data_test_bin$varObjBin, predict(modelo3b,data_test_bin,type = "response"), direction="<")
sensEspCorte(modelo3b,data_train_bin,"varObjBin",0.47,"1")
sensEspCorte(modelo3b,data_test_bin,"varObjBin",0.47,"1")

###### Step

todo<-data.frame(input_bin,varObjBin)

graficoVcramer(input_bin,varObjBin)

set.seed(45832)
trainIndex4 <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train_bin2 <- todo[trainIndex4,]
data_test_bin2 <- todo[-trainIndex4,]

null<-glm(varObjBin~1,data=data_train_bin2,family=binomial)

full<-glm(varObjBin~., data=data_train_bin2[,c(1:38,57)],family = binomial) 

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), trace=0,direction="both")
summary(modeloStepAIC)
pseudoR2(modeloStepAIC,data_train_bin2,"varObjBin")

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), trace=0,direction="backward")
summary(modeloBackAIC)
pseudoR2(modeloBackAIC,data_train_bin2,"varObjBin")

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), trace=0,direction="both",k=log(nrow(data_train_bin2)))
summary(modeloStepBIC)
pseudoR2(modeloStepBIC,data_train_bin2,"varObjBin")

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), trace=0, direction="backward",k=log(nrow(data_train_bin2)))
summary(modeloBackBIC)
pseudoR2(modeloBackBIC,data_train_bin2,"varObjBin")

modeloStepAIC$rank#AIC both es mas bajo y menos coeficientes (34)
modeloBackAIC$rank

pseudoR2(modeloStepAIC,data_train_bin2,"varObjBin") #alto r2, probablemente el mejor de esta tanda.
pseudoR2(modeloBackAIC,data_train_bin2,"varObjBin")
pseudoR2(modeloStepBIC,data_train_bin2,"varObjBin")
pseudoR2(modeloBackBIC,data_train_bin2,"varObjBin")

#los modelos AIC tienen mejor Rsquared pero mas coeficientes, puede ser que no valga la pena la mejora de r2 ante la mayor complejidad del modelo.

formInt<-formulaInteracciones(todo[,c(1:38,57)],39)
fullInt<-glm(formInt, data=data_train_bin2,family = binomial)

#Esto lleva mucho tiempo
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modeloStepAIC_int)
pseudoR2(modeloStepAIC_int,data_train_bin2,"varObjBin")

modeloBackAIC_int<-step(full, scope=list(lower=null, upper=fullInt), trace=0,direction="backward")
summary(modeloBackAIC_int)
pseudoR2(modeloBackAIC_int,data_train_bin2,"varObjBin")

modeloStepAIC_int$rank #muchos más parámetros
modeloBackAIC_int$rank #este tiene un cuarto de los parametros, elijo este aunque el r2 sea menor

# purebo con las transformadas

fullT<-glm(varObjBin~., data=data_train_bin2, family = binomial)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
pseudoR2(modeloStepAIC_trans,data_train_bin2,"varObjBin")

modeloBackAIC_trans<-step(null, scope=list(lower=null, upper=fullT), trace=0,direction="backward")
summary(modeloBackAIC_trans)
pseudoR2(modeloBackAIC_trans,data_train_bin2,"varObjBin")

modeloBackAIC_trans$rank 
modeloBackAIC_trans$rank #la diferencia de parametros no es tan grande como la anterior, pero igual BIC es menor, igual pruebo con los dos a ver que onda

#con interacciones y transformaciones

formIntT<-formulaInteracciones(todo,57)
fullIntT<-lm(formIntT, data=data_train_bin2)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
pseudoR2(modeloStepAIC_transInt,data_train_bin2,"varObjBin")

modeloBackAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), trace=0,direction="backward")
summary(modeloBackAIC_transInt)
pseudoR2(modeloBackAIC_transInt,data_train_bin2,"varObjBin")

modeloBackAIC_transInt$rank # este tiene demasiados coeficientes, igual que el AIC_trans, mejor ir con el otro.
modeloBackAIC_transInt$rank

############ VOLVER A ESTO, ESTO NO ESTA TERMINADO Pruebo los mejores de cada con validacion cruzada repetida
total<-c()
modelos<-sapply(list(modelo3b,modelo4b,modeloStepAIC,modeloBackAIC,modeloStepAIC_int,modeloBackAIC_int,modeloStepAIC_trans,modeloBackAIC_trans,modeloStepAIC_transInt,modeloBackAIC_transInt),formula)

auxVarObj<-datos_EED$varObjBin

datos_EED$varObjBin<-make.names(datos_EED$varObjBin)

for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = datos_EED,
             method = "glm", family=binomial,metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
datos_EED$varObjBin<-auxVarObj
boxplot(ROC ~modelo,data=total,main="Accuracy ")

#en la validacion cruzada salen como mejores modelos el modeloStepBIC_Int y modeloStepBIC_transInt, tinen 47 y 46 coeficientes respectivamente.

length(coef(modeloStepBIC_int))
length(coef(modeloStepBIC_transInt))

formula(modeloStepBIC_int)
formula(modeloStepBIC_transInt)

###### Step

todo<-data.frame(input_bin,varObjBin)

graficoVcramer(input_bin,varObjBin)

set.seed(45832)
trainIndex4 <- createDataPartition(todo$varObjBin, p=0.8, list=FALSE)
data_train_bin2 <- todo[trainIndex4,]
data_test_bin2 <- todo[-trainIndex4,]

null<-glm(varObjBin~1,data=data_train_bin2,family=binomial)

full<-glm(varObjBin~., data=data_train_bin2[,c(1:38,55)],family = binomial) 

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), trace=0,direction="both")
summary(modeloStepAIC)
pseudoR2(modeloStepAIC,data_train_bin2,"varObjBin")

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), trace=0,direction="backward")
summary(modeloBackAIC)
pseudoR2(modeloBackAIC,data_train_bin2,"varObjBin")

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), trace=0,direction="both",k=log(nrow(data_train_bin2)))
summary(modeloStepBIC)
pseudoR2(modeloStepBIC,data_train_bin2,"varObjBin")

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), trace=0, direction="backward",k=log(nrow(data_train_bin2)))
summary(modeloBackBIC)
pseudoR2(modeloBackBIC,data_train_bin2,"varObjBin")

modeloStepAIC$rank#AIC both es mas bajo y menos coeficientes (34)
modeloBackAIC$rank

pseudoR2(modeloStepAIC,data_train_bin2,"varObjBin") #alto r2, probablemente el mejor de esta tanda.
pseudoR2(modeloBackAIC,data_train_bin2,"varObjBin")
pseudoR2(modeloStepBIC,data_train_bin2,"varObjBin")
pseudoR2(modeloBackBIC,data_train_bin2,"varObjBin")

#los modelos AIC tienen mejor Rsquared pero mas coeficientes, puede ser que no valga la pena la mejora de r2 ante la mayor complejidad del modelo.

formInt<-formulaInteracciones(todo[,c(1:38,55)],39)
fullInt<-glm(formInt, data=data_train_bin2,family = binomial)

#Esto lleva mucho tiempo
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
summary(modeloStepAIC_int)
pseudoR2(modeloStepAIC_int,data_train_bin2,"varObjBin")

modeloStepBIC_int<-step(full, scope=list(lower=null, upper=fullInt), trace=0, direction="both",k=log(nrow(data_train_bin2)))
summary(modeloStepBIC_int)
pseudoR2(modeloStepBIC_int,data_train_bin2,"varObjBin")

modeloStepAIC_int$rank #muchos más parámetros
modeloStepBIC_int$rank #este tiene un cuarto de los parametros, elijo este aunque el r2 sea menor

# purebo con las transformadas

fullT<-glm(varObjBin~., data=data_train_bin2, family = binomial)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
pseudoR2(modeloStepAIC_trans,data_train_bin2,"varObjBin")

modeloStepBIC_trans<-step(full, scope=list(lower=null, upper=fullT), trace=0, direction="both",k=log(nrow(data_train_bin2)))
summary(modeloStepBIC_trans)
pseudoR2(modeloStepBIC_trans,data_train_bin2,"varObjBin")

modeloStepAIC_trans$rank 
modeloStepBIC_trans$rank #la diferencia de parametros no es tan grande como la anterior, pero igual BIC es menor, igual pruebo con los dos a ver que onda

#con interacciones y transformaciones

formIntT<-formulaInteracciones(todo,55)
fullIntT<-lm(formIntT, data=data_train_bin2)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
pseudoR2(modeloStepAIC_transInt,data_train_bin2,"varObjBin")

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), trace=0, direction="both",k=log(nrow(data_train_bin2)))
summary(modeloStepBIC_transInt)
pseudoR2(modeloStepBIC_transInt,data_train_bin2,"varObjBin")

modeloStepBIC_transInt$rank # este tiene demasiados coeficientes, igual que el AIC_trans, mejor ir con el otro.
modeloStepBIC_transInt$rank

############ Pruebo los mejores de cada con validacion cruzada repetida
total<-c()
modelos<-sapply(list(modelo3b,modelo4b,modeloStepAIC,modeloStepBIC),formula)

auxVarObj<-datos_EED$varObjBin

datos_EED$varObjBin<-make.names(datos_EED$varObjBin)

for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = datos,
             method = "glm", family=binomial,metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
datos_EED$varObjBin<-auxVarObj
boxplot(ROC ~modelo,data=total,main="Accuracy ")

aggregate(ROC ~modelo, data = total, mean)
aggregate(ROC ~modelo, data = total, sd)

#en la validacion cruzada salen como mejores modelos el modeloStepBIC_Int y modeloStepBIC_transInt, tinen 47 y 46 coeficientes respectivamente.

length(coef(modeloStepBIC_int))
length(coef(modeloStepBIC_transInt))

formula(modeloStepBIC_int)
formula(modeloStepBIC_transInt)

modeloStepAIC_trans$rank
modeloStepBIC_trans$rank
