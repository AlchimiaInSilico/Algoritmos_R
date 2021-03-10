options(java.parameters = "-Xmx4096m")
setwd("C:/Imbalanced/codigo")
library(readxl)
library(readr)
library(RWeka)
library(rlist)
library(FNN)
library(Metrics)
library(caret)
library(mccr)
source("Sampleo.R")
source("prepararBD.R")
source("creacionModelos.R")
source("calculoAD.R")
source("prediccionPorConsenso.R")
source("metricasResultados.R")

#cant de train 80%
cantTrain<-0.8

#Umbral minimo de Accuracy para considerar prediccion
umbralMinimoAccuracy<-0

#cantidad de modelos por sampleo
cantModelos<-10
umbralCVModelos<-0.6


#### CREACION DE MODELOS #######
#Opciones de Algoritmo de aprendizaje: "RandomForest", "MultilayerPerceptron","NaiveBayes", "RandomCommittee"
#Opciones de Metodo de Sampleo: "HybridMethodSampling", "OneSidedRandomSampling", "sinSampleo"
algoritmoAprendizaje<-"RandomForest"
metodoSampleo<-"HybridMethodSampling"


AID504466_DRAGON <- read_csv("C:/imbalanced/AID_485314_filtrada.csv")
AID504466_DRAGON<-cbind(AID504466_DRAGON[,-1], AID504466_DRAGON[,1])
SID<-1:nrow(AID504466_DRAGON)
dataBase<-cbind(SID,AID504466_DRAGON)

#elimino inconclusives
dataBase<-dataBase[dataBase$Clase!="Unspecified",]
dataBase<-dataBase[dataBase$Clase!="Inconclusive",]
#train y test se dividen por random en 80:20
set.seed(99)
trainID<-1:nrow(dataBase)
samp <- sample(trainID, round(nrow(dataBase)*cantTrain))
trainTOTAL<-dataBase[samp,]
testTOTAL<-dataBase[-samp,]

dataBase<-trainTOTAL
test<-testTOTAL

# colnames(dataBase)<-gsub("[[:punct:]]","",colnames(dataBase))
# colnames(test)<-gsub("[[:punct:]]","",colnames(test))

dataBase$Clase <-gsub("\\<Active\\>","HIGH",dataBase$Clase)
dataBase$Clase<-gsub("\\<Inactive\\>","LOW",dataBase$Clase)
test$Clase<-gsub("\\<Active\\>","HIGH",test$Clase)
test$Clase<-gsub("\\<Inactive\\>","LOW",test$Clase)



dataBase$Clase<-as.factor(dataBase$Clase)
test$Clase<-as.factor(test$Clase)


activos<-dataBase[dataBase$Clase=="HIGH",]
inactivos<-dataBase[dataBase$Clase=="LOW",]

comienzoDescriptoresTest<-2
comienzoDescriptoresTrain<-2

AlgoritmoImbalanced<-function(umbralMinimoAccuracy,algoritmoAprendizaje,metodoSampleo){

  #prepararBD(dataBase,compAsamplear,0.8,Umbral,etiqueta1,etiqueta2)

  #### CREACION DE MODELOS #######
  #Opciones de Algoritmo de aprendizaje: "RandomForest", "MultilayerPerceptron","NaiveBayes", "RandomCommittee"
  #Opciones de Metodo de Sampleo: "HybridMethodSampling", "OneSidedRandomSampling", "sinSampleo"
  tiempoMODELOS<-system.time({
    creacionModelos(cantModelos,algoritmoAprendizaje,metodoSampleo)
  })

  #### APPLICABILITY DOMAIN ####
  tiempoAD<-system.time({
    calculoAD()
  })

  #PREDICCION POR CONSENSO#
  for(i in 1:length(umbralMinimoAccuracy)){
    prediccionPorConsenso(umbralMinimoAccuracy[i])

    #### METRICAS DE DESEMPEÑO Y ESCRITURA DE RESULTADOS #####
    metricasResultados(umbralMinimoAccuracy[i])

  }
}

# setwd("../")
# dir.create("RandomForest_HybridMethodSampling_2021DBnueva")
# setwd("RandomForest_HybridMethodSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomForest","HybridMethodSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")

setwd("../")
dir.create("RandomForest_OneSidedRandomSampling_2021DBnueva")
setwd("RandomForest_OneSidedRandomSampling_2021DBnueva")
AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomForest","OneSidedRandomSampling")
save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")

setwd("../")
dir.create("RandomForest_sinSampleo_2021DBnueva")
setwd("RandomForest_sinSampleo_2021DBnueva")
AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomForest","sinSampleo")
save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")

# 
# #####
# setwd("../")
# dir.create("MultilayerPerceptron_HybridMethodSampling_2021DBnueva")
# setwd("MultilayerPerceptron_HybridMethodSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"MultilayerPerceptron","HybridMethodSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# setwd("../")
# dir.create("MultilayerPerceptron_OneSidedRandomSampling_2021DBnueva")
# setwd("MultilayerPerceptron_OneSidedRandomSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"MultilayerPerceptron","OneSidedRandomSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# 
# setwd("../")
# dir.create("MultilayerPerceptron_sinSampleo_2021DBnueva")
# setwd("MultilayerPerceptron_sinSampleo_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"MultilayerPerceptron","sinSampleo")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# #####
# setwd("../")
# dir.create("NaiveBayes_HybridMethodSampling_2021DBnueva")
# setwd("NaiveBayes_HybridMethodSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"NaiveBayes","HybridMethodSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# setwd("../")
# dir.create("NaiveBayes_OneSidedRandomSampling_2021DBnueva")
# setwd("NaiveBayes_OneSidedRandomSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"NaiveBayes","OneSidedRandomSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# setwd("../")
# dir.create("NaiveBayes_sinSampleo")
# setwd("NaiveBayes_sinSampleo")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"NaiveBayes","sinSampleo")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# #####
# setwd("../")
# dir.create("RandomCommittee_HybridMethodSampling_2021DBnueva")
# setwd("RandomCommittee_HybridMethodSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomCommittee","HybridMethodSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# setwd("../")
# dir.create("RandomCommittee_OneSidedRandomSampling_2021DBnueva")
# setwd("RandomCommittee_OneSidedRandomSampling_2021DBnueva")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomCommittee","OneSidedRandomSampling")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
# 
# setwd("../")
# dir.create("RandomCommittee_sinSampleo")
# setwd("RandomCommittee_sinSampleo")
# AlgoritmoImbalanced(c(0,0.2,0.4,0.6,0.8),"RandomCommittee","sinSampleo")
# save(list=ls()[-c(grep("dataBase",ls()),grep("test",ls()),grep("activos",ls()),grep("inactivos",ls()))], file="mylocals.Rda")
