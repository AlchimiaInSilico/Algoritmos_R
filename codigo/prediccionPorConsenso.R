prediccionPorConsenso<-function(umbralMinimoAccuracy){
  matrizDePredicciones<-c()
  for(modelo in 1:length(modelos)){
    prediccion<-predict(modelos[[modelo]],test[,c(FSdeModelos[[modelo]])])
    prediccion<-gsub("\\<HIGH\\>",1,prediccion)
    prediccion<-gsub("\\<LOW\\>",-1,prediccion)
    matrizDePredicciones<-cbind(matrizDePredicciones,prediccion)
  }
  
  #la prediccion por consenso será el mayor entre la sumatoria de predicciones clase1 y
  #la sumatoria de predicciones clase2, donde cada valor de la sumatoria esta dado por:
  #(accuracyModelo * peso de AD (1-AD) * valor de prediccion) correspondiente a cada modelo
  clasePredicha<-c()
  accuracyPrediccion<-c()
  for(compuestoTesteo in 1:nrow(test)){
    sumatoriaClase1<-0 #corresponde a la clase HIGH
    sumatoriaClase2<-0 #corresponde a la clase LOW
    modelosClase1<-0  #corresponde a la clase HIGH
    modelosClase2<-0 #corresponde a la clase LOW
    for(modelo in 1:length(modelos)){
      accuracyModelo<-accuracyModelos[modelo]
      pesoAD<-matrizDeAD[compuestoTesteo,(modelo+1)]
      if(pesoAD>0){ #si pesoAD==0, ese compuesto para ese modelo está fuera de AD
        valorPrediccion<-as.numeric(matrizDePredicciones[compuestoTesteo,modelo])
        if(valorPrediccion==1){
          sumatoriaClase1<-sumatoriaClase1+(accuracyModelo*pesoAD*as.numeric(valorPrediccion))
          modelosClase1<-modelosClase1+1
        }
        if(valorPrediccion==-1){
          sumatoriaClase2<-sumatoriaClase2+(accuracyModelo*pesoAD*as.numeric(valorPrediccion))
          modelosClase2<-modelosClase2+1
        }
      }
    }
    
    promedioClase1<-0
    promedioClase2<-0
    if(sumatoriaClase1!=0 || sumatoriaClase2!=0){ #NO esta fuera de AD
      if(modelosClase1>0)
        promedioClase1<-sumatoriaClase1/modelosClase1
      if(modelosClase2>0)
        promedioClase2<-sumatoriaClase2/modelosClase2
    }
    
    prediccionConsenso<-promedioClase1+promedioClase2
    if(promedioClase1==0 && promedioClase2==0){
      clasePredicha<-rbind(clasePredicha,"FUERA DE DOMINIO")
      accuracyPrediccion<-rbind(accuracyPrediccion,NA)
    } else {
      if(prediccionConsenso<0){
        if(abs(promedioClase2)>umbralMinimoAccuracy){
          clasePredicha<-rbind(clasePredicha,"LOW")
          accuracyPrediccion<-rbind(accuracyPrediccion,abs(promedioClase2))
        } else {
          clasePredicha<-rbind(clasePredicha,"NoSuperaUmbral")
          accuracyPrediccion<-rbind(accuracyPrediccion,NA)
        }
      } else {
        if(prediccionConsenso>0){
          if(abs(promedioClase1)>umbralMinimoAccuracy){
            clasePredicha<-rbind(clasePredicha,"HIGH")
            accuracyPrediccion<-rbind(accuracyPrediccion,abs(promedioClase1))
          } else {
            clasePredicha<-rbind(clasePredicha,"NoSuperaUmbral")
            accuracyPrediccion<-rbind(accuracyPrediccion,NA)
          }
        } else {
          clasePredicha<-rbind(clasePredicha,"INDETERMINADO")
          accuracyPrediccion<-rbind(accuracyPrediccion,NA)
        }
      }
    }
  }
  
  
  PrediccionFinal<-data.frame(cbind(test$SID,clasePredicha,accuracyPrediccion))
  colnames(PrediccionFinal)<-c("SID","Clase","Accuracy")
  contadorTotal<-0
  contadorCC<-0 #Correctamente Clasificados
  for(i in 1:nrow(clasePredicha)){
    if(clasePredicha[i]!= "INDETERMINADO" && clasePredicha[i]!= "FUERA DE DOMINIO" && clasePredicha[i]!= "NoSuperaUmbral"){
      contadorTotal<-contadorTotal+1
      if(clasePredicha[i]==test$Clase[i]){
        contadorCC<-contadorCC+1
      }
    }
    else i<-i+1
  }
  
  totalCorrecClasificados<-contadorCC/contadorTotal 
  
  assign("PrediccionFinal",PrediccionFinal,envir = .GlobalEnv)
}