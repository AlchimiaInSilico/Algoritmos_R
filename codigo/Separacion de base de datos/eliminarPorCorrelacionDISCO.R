library(data.table)
library(dplyr)

leerColumnaDeCSV<-function(archivo, columna){
  fread(archivo, select = columna)
}


#armo la matriz de correlaciones desde el archivo del dataSet
armarMatriz<-function(dataSet,umbralMAX,umbralMIN){
  cantidadDescriptores<-length(fread(dataSet,nrows = 1))
  ventanaAtraer<-3
  necesarioTraer<-0
  if(ventanaAtraer<=cantidadDescriptores){
    #inicializo la matriz de correlaciones y la que mapeare (valores 0, 0.45 y 1)
    matriz<-array(NA,c(cantidadDescriptores,cantidadDescriptores))
    matrizMapeada<-array(NA,c(cantidadDescriptores,cantidadDescriptores))
    if(cantidadDescriptores>1){
      for(d1 in 1:(cantidadDescriptores-1)){
        print(d1)
        indice2<-2
        minDescriptor<-d1
        if((minDescriptor+ventanaAtraer)<cantidadDescriptores){
          maxDescriptor<-minDescriptor+ventanaAtraer 
        } else {
          maxDescriptor<-cantidadDescriptores
        }
        #traigo la ventana de descriptores desde el csv
        descriptoresTraidos<-leerColumnaDeCSV(dataSet,c(minDescriptor:maxDescriptor))
        descriptor1<-as.numeric(unlist(descriptoresTraidos[,1,with=FALSE])) 
        d2<-d1+1
        while(d2<=cantidadDescriptores){
          if(necesarioTraer==1){
            minDescriptor<-maxDescriptor+1
            if((maxDescriptor+ventanaAtraer)<cantidadDescriptores){
              maxDescriptor<-maxDescriptor+ventanaAtraer 
            } else {
              maxDescriptor<-cantidadDescriptores
            }
            descriptoresTraidos<-leerColumnaDeCSV(dataSet,c(minDescriptor:maxDescriptor))
            necesarioTraer<-0
            indice2<-1
          }
          descriptor2<-as.numeric(unlist(descriptoresTraidos[,indice2,with=FALSE])) 
          
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
          indice2<-indice2+1
          if(d2>maxDescriptor && maxDescriptor<cantidadDescriptores){
            necesarioTraer<-1
          }
        }
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

#como no se puede leer columnas de disco y escribir a disco (dependiendo de
#si hay que considerar dichas columnas), entonces leo una columna, la escribo como
#fila y repito la operacion para dejar compuestos en filas y descriptores en columnas
nuevoCSV<-function(archivoOriginal,archivoNuevo,descriptoresAconsiderar){
  for(i in 1:length(descriptoresAconsiderar)){
    columna<-leerColumnaDeCSV(archivoOriginal,descriptoresAconsiderar[i])
    columna<-data.frame(cbind(colnames(columna),t(columna)))
    fwrite(columna,append = TRUE, file = "archivoNuevo.csv")
  }
  cantidadFilas<-nrow(leerColumnaDeCSV(archivoOriginal,descriptoresAconsiderar[i]))
  for(i in 1:cantidadFilas){
    columna<-leerColumnaDeCSV("archivoNuevo.csv",i)
    columna<-data.frame(t(columna))
    fwrite(columna,append = TRUE, file = archivoNuevo, col.names = FALSE)
  }
  if (file.exists("archivoNuevo.csv")) file.remove("archivoNuevo.csv")
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

write.csv(leerColumnaDeCSV("baseDatosLeishFiltrada.csv",c(1:15)),"Libro1.csv")

tiempo<-system.time({
  matrizCorrelacionesMapeada<-armarMatriz("Libro1.csv",0.95,0.90)
  matrizSinCorrelaciones<-eliminarDescriptoresCorrelacionados(matrizCorrelacionesMapeada)
  #lista de descriptores que no estan tan correlacionados
  descriptoresAconsiderar<-as.integer(rownames(matrizSinCorrelaciones))
  #armo un nuevo csv dejando solo los descriptoresAconsiderar
  nuevoCSV("Libro1.csv","DBSinCorrelaciones.csv",descriptoresAconsiderar)
})

