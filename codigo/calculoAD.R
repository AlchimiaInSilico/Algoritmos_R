calculoAD<-function(){
  #busco los 3 vecinos mas cercanos a cada compuesto de testeo
  trainSampleado<-dataBase[dataBase$SID%in%DBdeModelos.SIDs[,1],]
  vecinosMasCercanosK3<-get.knnx(trainSampleado[,comienzoDescriptoresTrain:(ncol(trainSampleado)-1)],test[,comienzoDescriptoresTest:(ncol(test)-1)],k=3)
  #creo una matriz de (SID x Modelos) donde para cada SID guardo 0 si no pasa el AD
  #y guardo el valor de AD y la prediccion si lo pasa
  matrizDeAD<-matrix(data=0, nrow(test),(length(modelos)+1))
  #para cada modelo veo que compuestos a testear pasan el AD. Retorno una matriz (SID x Modelos)
  for(modelo in 1:length(modelos)){
    print("AD del modelo: ")
    print(modelo)
    #calculo el RMSE de entrenamiento para el modelo
    trainSampleadoFiltradoFS<-trainSampleado[,c(FSdeModelos[[modelo]])]
    prediccionTrainSampleado<-evaluate_Weka_classifier(modelos[[modelo]],trainSampleadoFiltradoFS)
    MAEtrainSampleado<-prediccionTrainSampleado$details[5]
    
    for(compuestoTesteo in 1:nrow(test)){
      #de los 3 vecinos mas cercanos tengo el ID, por lo tanto tengo que mapear a su correspondiente
      #SID y luego ir a filtrar a la dataBase por SID de los 3 vecinos
      vecinosCercanos<-dataBase[dataBase$SID%in%trainSampleado[vecinosMasCercanosK3$nn.index[compuestoTesteo,],1][[1]],c(FSdeModelos[[modelo]])]
      #predigo los 3 vecinos y promedio los casos correctamente clasificados respecto del total
      prediccion3vecinos<-evaluate_Weka_classifier(modelos[[modelo]],vecinosCercanos)
      promedioCC3Vecinos<-prediccion3vecinos$details[1]
      
      #calculo el AD (MAE3vecinos/RMSEtrain)
      MAE3vecinos<-prediccion3vecinos$details[5] 
      if(MAEtrainSampleado!=0){
        AD<-MAE3vecinos/MAEtrainSampleado  
      }else{ #Si todo el train no tiene error
        if(MAE3vecinos==0){
          AD<-0 #dentro de AD porque sus 3 vecinos tampoco tienen error
        }else{
          AD<-999 #fuera de AD porque sus 3 vecinos tienen error y el train no error
        }
      }
      
      
      
      matrizDeAD[compuestoTesteo,1]<-test$SID[compuestoTesteo]
      #si el RMSE de los 3vecinos es menor que el RMSE del train, el valor real de pertenencia
      #al AD sera (1-MAE3vecinos/MAEtrainSampleado)
      if(AD<1){
        matrizDeAD[compuestoTesteo,(modelo+1)]<-(1-AD)
      }
    }
  }
  assign("matrizDeAD",matrizDeAD,envir = .GlobalEnv)
}
