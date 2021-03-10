metricasResultados<-function(umbralMinimoAccuracy){
  actual<-test[test$SID%in%PrediccionFinal[!is.na(PrediccionFinal$Accuracy),]$SID,]$Clase
  predicted<-PrediccionFinal[!is.na(PrediccionFinal$Accuracy),2]
  actual<-factor(actual,levels = c("HIGH","LOW"))
  predicted<-factor(predicted,levels = c("HIGH","LOW"))
  CC<-sum(actual==predicted)/length(actual)
  
  cantHIGH<-sum(actual=="HIGH")
  cantLOW<-sum(actual=="LOW")
  
  ##Matriz de confusion
  confusionMatrix<-confusionMatrix(actual,predicted)
  
  ##para Matthew
  actual.matthew<-gsub("\\<HIGH\\>",1,actual)
  actual.matthew<-gsub("\\<LOW\\>",0,actual.matthew)
  predicted.matthew<-gsub("\\<HIGH\\>",1,predicted)
  predicted.matthew<-gsub("\\<LOW\\>",0,predicted.matthew)
  matthewCC<-mccr(actual.matthew,predicted.matthew)
  
  ####### Resultados ########
  
  fileConn<-file(paste("Umbral-",umbralMinimoAccuracy,"_resultados.txt",sep = ""))
  writeLines(c("Resultados de la experimentacion con los siguientes parametros: ","\n","\n",
               
               "Umbral minimo de Accuracy para considerar prediccion: ",umbralMinimoAccuracy,"\n",
               "Cantidad de modelos por sampleo: ",cantModelos,"\n",
               "Umbral Cross-Validation de modelos: ",umbralCVModelos,"\n",
               "Algoritmo de aprendizaje: ",algoritmoAprendizaje,"\n",
               "Metodo de Sampleo: ",metodoSampleo,"\n","\n",
               
               "Accuracy de los modelos: ","\n",
               accuracyModelos[1],"\n",accuracyModelos[2],"\n",accuracyModelos[3],"\n",accuracyModelos[4],"\n",accuracyModelos[5],"\n",
               accuracyModelos[6],"\n",accuracyModelos[7],"\n",accuracyModelos[8],"\n",accuracyModelos[9],"\n",accuracyModelos[10],"\n",
               #"Tiempo en horas: ",round((tiempoAD[3]+tiempoMODELOS[3])/3600,2),"\n",
               "\n",
               "FS de los modelos: ","\n",
               gsub("Clase","",FSdeModelos[1]),"\n",gsub("Clase","",FSdeModelos[2]),"\n",
               gsub("Clase","",FSdeModelos[3]),"\n",gsub("Clase","",FSdeModelos[4]),"\n",
               gsub("Clase","",FSdeModelos[5]),"\n",gsub("Clase","",FSdeModelos[6]),"\n",
               gsub("Clase","",FSdeModelos[7]),"\n",gsub("Clase","",FSdeModelos[8]),"\n",
               gsub("Clase","",FSdeModelos[9]),"\n",gsub("Clase","",FSdeModelos[10]),"\n",
               
               "\n","################################################################################","\n","\n","\n","\n",
               
               "\n","Evaluacion de la clasificacion","\n",
               "################################################################################","\n","\n",
               
               "Matriz De Confusion","\n",
               "\t","LOW","\t","HIGH","\n",
               "LOW","\t",confusionMatrix$table[1],"\t",confusionMatrix$table[3],"\n",
               "HIGH","\t",confusionMatrix$table[2],"\t",confusionMatrix$table[4],"\n","\n",
               
               "Cantidad de Activos que pasaron el AD: ",cantHIGH,"\n",
               "Cantidad de Inactivos que pasaron el AD: ",cantLOW,"\n",
               "Cantidad TOTAL que pasaron el AD: ",cantHIGH+cantLOW,"\n","\n",
               
               "Porcentaje correctamente clasificados ","\n",
               "Global: ",round(CC*100,2),"%","\n",
               #"Descriptores que pertenecen al target y fueron seleccionados por el FS: ",round(matrizDeConfusion[1,1]/sum(matrizDeConfusion[1,])*100,2),"%","\n",
               #"Descriptores que no pertenecen al target y no fueron seleccionados por el FS: ",round(matrizDeConfusion[2,2]/sum(matrizDeConfusion[2,])*100,2),"%","\n","\n",
               
               "Sensitividad: ",round(confusionMatrix$byClass[1]*100,2),"%","\n","\n",
               "Especificidad: ",round(confusionMatrix$byClass[2]*100,2),"%","\n","\n",
               #"Carvalho and Freitas (sigma): ",round((matrizDeConfusion[1,1]/sum(matrizDeConfusion[,1])*matrizDeConfusion[2,2]/sum(matrizDeConfusion[,2]))*100,2),"%","\n",
               "----------------------------------------","\n",
               "Matthew: ",round(matthewCC*100,2),"%","\n","\n",
               "\n","################################################################################","\n","\n","\n","\n",
               ""
  ),sep = "", fileConn)
  close(fileConn)
  
  
}