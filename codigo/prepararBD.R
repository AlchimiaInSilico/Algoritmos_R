agregarEtiquetaClase<-function(database, umbral, etiq1, etiq2){
  niveles<-c(-Inf,50,Inf)
  clases<-c(etiq1,etiq2)
  database$Clase <- cut(database$HTS, niveles, clases)
  return(database)
}

prepararBD<-function(dataBase,compAsamplear,cantTrain,Umbral,etiqueta1,etiqueta2){
  
  #sampleo random cierta cantidad de compuestos
  dataBase<-dataBase[sample(nrow(dataBase), compAsamplear), ]
  colnames(dataBase)[5]<-"HTS"
  #le agrego la columna con la etiqueta de clase
  dataBase<-agregarEtiquetaClase(dataBase,Umbral,etiqueta1,etiqueta2)
  #train y test se dividen por random en 80:20 (solo guardo columnas SID, HTS y Clase)
  trainID<-1:nrow(dataBase)
  samp <- sample(trainID, round(nrow(dataBase)*cantTrain))
  trainTOTAL<-dataBase[samp,c("SID","HTS","Clase")]
  
  #divido entre activos e inactivos
  inactivos<-trainTOTAL[trainTOTAL$HTS<=Umbral,]
  activos<-trainTOTAL[trainTOTAL$HTS>Umbral,]
  #en database dejo SID, descriptores, Clase. Las demas columnas las guardo
  columnasAguardar<-c(1,2,4:7)
  columnasGuardadas<-dataBase[,columnasAguardar]
  dataBase<-dataBase[,-columnasAguardar]
  
  #colnames(dataBase)<-gsub("[[:punct:]]","",colnames(dataBase))
  #colnames(test)<-gsub("[[:punct:]]","",colnames(test))
  
  assign("comienzoDescriptoresTest",2,envir = .GlobalEnv)  #columna donde comienzan los descriptores del test
  assign("comienzoDescriptoresTrain",2,envir = .GlobalEnv) #columna donde comienzan los descriptores del train
  
  assign("dataBase",dataBase[samp,],envir = .GlobalEnv)  #BD de entrenamiento (el 80%)
  assign("test",dataBase[-samp,],envir = .GlobalEnv)  #BD de testeo (el 20%)
  assign("activos",dataBase[dataBase$Clase=="HIGH",],envir = .GlobalEnv)
  
  assign("inactivos",dataBase[dataBase$Clase=="LOW",],envir = .GlobalEnv)
}
