library(data.table)
#library(dplyr)

#armo la matriz de correlaciones desde el archivo del dataSet
armarMatriz<-function(dataSet,umbralMAX,umbralMIN){
  cantidadDescriptores<-length(dataSet[1,])
  #inicializo la matriz de correlaciones y la que mapeare (valores 0, 0.45 y 1)
  matriz<-array(NA,c(cantidadDescriptores,cantidadDescriptores))
  matrizMapeada<-array(NA,c(cantidadDescriptores,cantidadDescriptores))
  if(cantidadDescriptores>1){
    for(d1 in 1:(cantidadDescriptores-1)){
      print(d1)
      descriptor1<-dataSet[,d1,with=FALSE]
      d2<-d1+1
      while(d2<=cantidadDescriptores){
        descriptor2<-dataSet[,d2,with=FALSE]
        correlacion<-abs(cor(descriptor1,descriptor2))
        matriz[d1,d2]<-correlacion
        matriz[d2,d1]<-correlacion
        
        ifelse(correlacion >= umbralMAX,{
          matrizMapeada[d1,d2]<-1
          matrizMapeada[d2,d1]<-1
        },
        ifelse(correlacion >= umbralMIN,{
          matrizMapeada[d1,d2]<-0.45
          matrizMapeada[d2,d1]<-0.45
        },
        ifelse(correlacion < umbralMIN,{
          matrizMapeada[d1,d2]<-0
          matrizMapeada[d2,d1]<-0
        })))
        
        d2<-d2+1
        
      }
    }
  }
  
  for(i in 1:cantidadDescriptores){
    matriz[i,i]<-1
    matrizMapeada[i,i]<-1
  }  
  matriz<-data.frame(matriz)
  matrizMapeada<-data.frame(matrizMapeada)
  colnames(matriz)<-1:cantidadDescriptores
  colnames(matrizMapeada)<-1:cantidadDescriptores
  write.csv(matriz,"matrizCorrelaciones.csv")
  write.csv(matrizMapeada,"matrizCorrelacionesMapeada.csv")
  return(matrizMapeada)
}


eliminarDescriptoresCorrelacionados<-function(matrizCorrelacionesMapeada){
  #mientras haya algun descriptor cuya correlacion supere el umbral
  while(sum(matrizCorrelacionesMapeada[lower.tri(matrizCorrelacionesMapeada)])>0){
    vectorSumatoriaCorrelaciones<-c()
    for(i in 1:length(matrizCorrelacionesMapeada)){
      vectorSumatoriaCorrelaciones<-c(vectorSumatoriaCorrelaciones,
                                      sum(matrizCorrelacionesMapeada[,i]))
    }
    posicionMayor<-which.max(vectorSumatoriaCorrelaciones)
    #elimino el descriptor que este mas correlacionado con todos los demas
    matrizCorrelacionesMapeada<-matrizCorrelacionesMapeada[-posicionMayor,-posicionMayor]
  }
  return(matrizCorrelacionesMapeada)
}



tiempo<-system.time({
  baseDatosLeishFiltrada<-fread("Libro1.csv")
  baseDatosLeishFiltrada<-baseDatosLeishFiltrada[,-c(1,2,3)]
  matrizCorrelacionesMapeada<-armarMatriz(baseDatosLeishFiltrada,0.95,0.90)
  matrizSinCorrelaciones<-eliminarDescriptoresCorrelacionados(matrizCorrelacionesMapeada)
  #lista de descriptores que no estan tan correlacionados
  descriptoresAconsiderar<-as.integer(rownames(matrizSinCorrelaciones))
  #armo un nuevo csv dejando solo los descriptoresAconsiderar
  baseDatosLeishSinCOR<-baseDatosLeishFiltrada[,descriptoresAconsiderar,with=FALSE]
  fwrite(baseDatosLeishSinCOR,file = "baseDatosLeishSinCOR.csv")
})

