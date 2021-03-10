#RUTA DONDE SE ENCUENTRA EL ARCHIVO MOL2
#setwd("C:/CARPETA/CARPETA")  

eliminarEspacios<-function(texto){
  tor<-c()
  texto<-strsplit(texto," ")
  for(i in 1:length(texto[[1]])){
    if(texto[[1]][i]!=""){
      tor<-c(tor,(texto[[1]][i]))
    }
  }
  return(tor)
}


corregirNOO<-function(molecules,molecula1,molecula2,molecula3,numEnlaceNuevo){
  cantidadLineas<-nrow(molecules)
  for(i in 1:cantidadLineas){
    #si encuentro un "N" en la linea i
    if(length(eliminarEspacios(molecules[i,1]))>5 &&
       eliminarEspacios(molecules[i,1])[6]==molecula1){
      #y si tambien encuentro "O" en las dos lineas siguientes
      if(eliminarEspacios(molecules[i+1,1])[6]==molecula2 && 
         eliminarEspacios(molecules[i+2,1])[6]==molecula3){
        #guardo el id del "N"
        id<-as.numeric(eliminarEspacios(molecules[i,1])[1])
        #avanzo filas hasta encontrar ese id en los bonds
        i<-i+1
        while(eliminarEspacios(molecules[i,1])!="@<TRIPOS>BOND"){
          i<-i+1
        }
        i<-i+1
        while(as.numeric(eliminarEspacios(molecules[i,1])[2])!=id){
          i<-i+1
        }
        #aqui estoy en el primer enlace NO, escribo un 2 en su correspondiente parametro
        filaOxigeno<-eliminarEspacios(molecules[i,1])
        filaOxigeno[4]<-numEnlaceNuevo
        molecules[i,1]<-paste(filaOxigeno[1],filaOxigeno[2],filaOxigeno[3],filaOxigeno[4])
        
        #aqui estoy en el primer enlace NO, escribo un 2 en el segundo enlace (i+1)
        filaOxigeno<-eliminarEspacios(molecules[i+1,1])
        filaOxigeno[4]<-numEnlaceNuevo
        molecules[i+1,1]<-paste(filaOxigeno[1],filaOxigeno[2],filaOxigeno[3],filaOxigeno[4])
      }
    }
  }
  return(molecules)
}
