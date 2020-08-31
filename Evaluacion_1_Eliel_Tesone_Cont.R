setwd('C:/Users/etesone/Desktop/MASTER UCM/Mineria de Datos y Modelizacion Predictiva/Evaluacion 1')

source("Funciones_R.R")

paquetes(c("questionr","psych","car","corrplot","readxl","ggplot2","gdata","caret","lmSupport","rpart","glmnet","dplyr","epiDisplay","pROC","reshape"))

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

datos_EE<-rename(datos_EE, c("Age_0-4_Ptge" = "Age_0to4_Ptge"))

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

arbol_dec<-rpart(varObjCont~datos_EE$Explotaciones,data=datos_EE) #Utilize esta funcion para decidir los puntos para separar las variables

arbol_dec

datos_EE$Explotaciones<-ifelse(datos_EE$Explotaciones< 78.5, 0, 1)

datos_EE$Industria<-ifelse(datos_EE$Industria< 34.20469, 0, 1)

datos_EE$totalEmpresas<-ifelse(datos_EE$totalEmpresas< 34.3927, 0, 1)

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

graficoVcramer(input_EED,varObjCont)

corrplot(cor(cbind(varObjCont,Filter(is.numeric, input_EED)), use="pairwise", 
             method="pearson"), method = "number",type = "upper")

#Todas las variables numéricas frente a la objetivo continua
graficoCorrelacion(varObjCont,input) #Nos fijamos en la forma de las líneas rojas (si hay muchas variables numéricas, tarda un poco)

##### Transformacion de variables numericas

input_cont<-cbind(input_EED,Transf_Auto(Filter(is.numeric, input_EED),varObjCont))

sapply(Filter(is.numeric, input_EED)[,-ncol(Filter(is.numeric, input_EED))],function(x) length(unique(x)))

saveRDS(data.frame(input_cont,varObjCont),"todo_cont_v")

todo_EE<-data.frame(input_EED,varObjCont)

######## Intento encontrar una regresion manualmente.

set.seed(342212)
trainIndex <- createDataPartition(todo_EE$varObjCont, p=0.8, list=FALSE)
data_train <- todo_EE[trainIndex,]
data_test <- todo_EE[-trainIndex,]

modeloCompleto<-lm(varObjCont~.,data=data_train)
summary(modeloCompleto)

Rsq(modeloCompleto,"varObjCont",data_train)
Rsq(modeloCompleto,"varObjCont",data_test)
#pruebo con las 3 primeras del vdecramer
modelo1<-lm(varObjCont~CCAA+Age_0to4_Ptge+SameComAutonDiffProvPtge,data=data_train)
summary(modelo1)

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test)
# pruebo con las primeras 5
modelo1a<-lm(varObjCont~CCAA+Age_0to4_Ptge+SameComAutonDiffProvPtge+AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
               ConstructionUnemploymentPtge+Explotaciones,data=data_train)
summary(modelo1a)

Rsq(modelo1a,"varObjCont",data_train)
Rsq(modelo1a,"varObjCont",data_test)

modelo2<-lm(varObjCont~CCAA+Age_0to4_Ptge+Age_19_65_pct+WomanPopulationPtge+ForeignersPtge+SameComAutonPtge+
              DifComAutonPtge+UnemployLess25_Ptge+AgricultureUnemploymentPtge+IndustryUnemploymentPtge+
              ConstructionUnemploymentPtge+ServicesUnemploymentPtge+ComercTTEHosteleria+ActividadPpal+inmuebles+
              SUPERFICIE+Densidad+PersonasInmueble+Industria+totalEmpresas,data=data_train)
summary(modelo2)

Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test)

modelo3<-lm(varObjCont~CCAA+Age_0to4_Ptge+Age_19_65_pct+WomanPopulationPtge+SameComAutonPtge+DifComAutonPtge+
              UnemployLess25_Ptge+AgricultureUnemploymentPtge+IndustryUnemploymentPtge+ConstructionUnemploymentPtge+
              ServicesUnemploymentPtge+ActividadPpal+inmuebles,data=data_train)
summary(modelo3)

Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test)

modelo4<-lm(varObjCont~CCAA+Age_0to4_Ptge+Age_19_65_pct+WomanPopulationPtge+SameComAutonPtge+DifComAutonPtge+CCAA:UnemployLess25_Ptge+
              AgricultureUnemploymentPtge+IndustryUnemploymentPtge+ConstructionUnemploymentPtge+
              ServicesUnemploymentPtge+ActividadPpal+inmuebles,data=data_train)
summary(modelo4)

Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test)

modelo5<-lm(varObjCont~CCAA+Age_0to4_Ptge+SameComAutonPtge+AgricultureUnemploymentPtge+ConstructionUnemploymentPtge:
              IndustryUnemploymentPtge+inmuebles+Densidad+CCAA:inmuebles+ActividadEconomica,data=data_train)
summary(modelo5)

Rsq(modelo5,"varObjCont",data_train)
Rsq(modelo5,"varObjCont",data_test)

##### Validacion Cruzada

modelo1VC <- train(formula(modelo1),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo1aVC <- train(formula(modelo1a),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)
modelo2VC <- train(formula(modelo2),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo3VC <- train(formula(modelo3),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo4VC <- train(formula(modelo4),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo5VC <- train(formula(modelo5),
                   data = todo_EE,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

results<-data.frame(rbind(modelo1VC$resample,modelo1aVC$resample,modelo2VC$resample,modelo3VC$resample,modelo4VC$resample,modelo5VC$resample),modelo=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100)))
boxplot(Rsquared~modelo,data=results) # modelos 3 o 5 son los mejores, respectivamente modelo2 y modelo4
aggregate(Rsquared~modelo, data = results, mean) #el 3 tiene mayor R2 medio
aggregate(Rsquared~modelo, data = results, sd)

# 44 coeficientes, es altisimo pero describe la variable muuucho mejor que el modelo1 y modelo1a.
#por otro lado el modelo 4 (modelo3) tiene un Rsquared bastante alto y 35 coeficientes y el alza de .002 o .003 en Rsquared no vale la pena por la cantidad de coef.
length(coef(modelo1));length(coef(modelo1a));length(coef(modelo2));length(coef(modelo3));length(coef(modelo4));length(coef(modelo5))  

#Ganador:
coef(modelo3)

Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test)

modelEffectSizes(modelo3)
barplot(sort(modelEffectSizes(modelo3)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)") 
# La variable mas importante es CCAA, lejos. El porcentaje de votos de izquierda depende mucho de la region

##### Modelos step

todo_cont<-data.frame(input_cont,varObjCont)
set.seed(131324)
trainIndex2 <- createDataPartition(todo_cont$varObjCont, p=0.8, list=FALSE)
data_train_cont <- todo_cont[trainIndex2,]
data_test_cont <- todo_cont[-trainIndex2,]

null<-lm(varObjCont~1, data=data_train_cont)

full<-lm(varObjCont~., data=data_train_cont[,c(1:38,57)]) #sin transformaciones

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
Rsq(modeloStepAIC,"varObjCont",data_test_cont)

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
summary(modeloBackAIC)
Rsq(modeloBackAIC,"varObjCont",data_test_cont) #son iguales

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
Rsq(modeloStepBIC,"varObjCont",data_test_cont) #Un pelin peor que el anterior, habrá que mirar el número de parámetros

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)
Rsq(modeloBackBIC,"varObjCont",data_test_cont) # son iguales

modeloStepAIC$rank
modeloStepBIC$rank

Rsq(modeloStepAIC,"varObjCont",data_train_cont)
Rsq(modeloBackAIC,"varObjCont",data_train_cont)
Rsq(modeloStepBIC,"varObjCont",data_train_cont)
Rsq(modeloBackBIC,"varObjCont",data_train_cont)

#los modelos AIC tienen mejor Rsquared pero mas coeficientes, puede ser que no valga la pena la mejora de r2 ante la mayor complejidad del modelo.

formInt<-formulaInteracciones(todo_cont[,c(1:38,57)],39)
fullInt<-lm(formInt, data=data_train_cont)

#Esto lleva mucho tiempo y resulta en demasiados coeficientes, mas de 200
#modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
#summary(modeloStepAIC_int)
#Rsq(modeloStepAIC_int,"varObjCont",data_test_cont) #Parecen algo mejores que los anteriores

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train_cont)))
summary(modeloStepBIC_int)
Rsq(modeloStepBIC_int,"varObjCont",data_test_cont) #el adjusted rsquared es mucho mejor

#modeloStepAIC_int$rank #muchos más parámetros
modeloStepBIC_int$rank #este tiene un cuarto de los parametros, elijo este aunque el r2 sea menor

# purebo con las transformadas

fullT<-lm(varObjCont~., data=data_train_cont)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
Rsq(modeloStepAIC_trans,"varObjCont",data_test_cont)#se acopla bien al adjusted r2 del test

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train_cont)))
summary(modeloStepBIC_trans)
Rsq(modeloStepBIC_trans,"varObjCont",data_test_cont) 

modeloStepAIC_trans$rank 
modeloStepBIC_trans$rank #la diferencia de parametros no es tan grande como la anterior, pero igual BIC es menor, igual pruebo con los dos a ver que onda

#con interacciones y transformaciones

formIntT<-formulaInteracciones(todo_cont,57)
fullIntT<-lm(formIntT, data=data_train_cont)

#Esto lleva mucho tiempo y resulta en demasiados coeficientes, mas de 200
#modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
#summary(modeloStepAIC_transInt)
#Rsq(modeloStepAIC_transInt,"varObjCont",data_test_cont) # se parece el valor a los int sin transf

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train_cont)))
summary(modeloStepBIC_transInt)
Rsq(modeloStepBIC_transInt,"varObjCont",data_test_cont) # 

#modeloStepAIC_transInt$rank # este tiene demasiados coeficientes, igual que el AIC_trans, mejor ir con el otro.
modeloStepBIC_transInt$rank

## Pruebo los mejores de cada con validacion cruzada repetida
total<-c()
modelos<-sapply(list(modelo3,modelo2,modeloStepAIC,modeloStepBIC,modeloStepBIC_int,modeloStepBIC_trans,modeloStepBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(43534)
  vcr<-train(as.formula(modelos[[i]]), data = data_train_cont,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
boxplot(Rsquared~modelo,data=total,main="R-Square") 
aggregate(Rsquared~modelo, data = total, mean) #el 5 y el 7 son mejores
aggregate(Rsquared~modelo, data = total, sd) #su variabilidad es algo más alta tb

#en la validacion cruzada salen como mejores modelos el modeloStepBIC_Int y modeloStepBIC_transInt, tinen 47 y 46 coeficientes respectivamente.

modelo3$rank
modelo2$rank
modeloStepAIC$rank
modeloStepBIC$rank
modeloStepBIC_int$rank
modeloStepBIC_trans$rank
modeloStepBIC_transInt$rank

Rsq(modelo3,"varObjCont",data_test_cont)
Rsq(modeloStepBIC,"varObjCont",data_test_cont)
Rsq(modeloStepBIC_trans,"varObjCont",data_test_cont)

length(coef(modelo3))
length(coef(modeloStepBIC_trans))

formula(modelo3)
formula(modeloStepBIC_trans)

#el mejor modelo es modeloStepBIC_trans porque tiene un r2 bueno y tiene 32 coeficientes. Modelos alternativos son el modeloStepBIC y modelo3.
#modelo 3 tiene un r2 mas alto pero tiene mas coeficientes aun, y el modeloStepBIC tiene un r2 una pisca mas bajo pero menos coeficientes. 
#El modelo modeloStepBIC_trans esta en el medio y ademas encuentra correlacion con las variables transformadas.
#Para refinar aun mas el modelo intentaria reemplazar variables en el modelo3 con las mismas variables transformadas para bajar 
#coeficientes aun mas y quizas subir el r2.
