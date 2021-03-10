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


corregirSOO<-function(molecules,molecula1,molecula2,molecula3,numEnlaceNuevo){
  cantidadLineas<-nrow(molecules)
  i<-1
  while(i<=cantidadLineas){
    #si encuentro un "S" en la linea i
    if(length(eliminarEspacios(molecules[i,1]))>5 &&
       eliminarEspacios(molecules[i,1])[6]==molecula1){
      #y si tambien encuentro "O" en las dos lineas siguientes
      if(eliminarEspacios(molecules[i+1,1])[6]==molecula2 && 
         eliminarEspacios(molecules[i+2,1])[6]==molecula3){
        filaAzufre<-i
        #retrocedo filas hasta llegar a @<TRIPOS>MOLECULE
        while(eliminarEspacios(molecules[i,1])!="@<TRIPOS>MOLECULE"){
          i<-i-1
        }
        #modifico la cantidad de atoms y bonds
        lineaCantidades<-eliminarEspacios(molecules[i+2,1])
        molecules[i+2,1]=paste(as.numeric(lineaCantidades[1])+1,
                                 as.numeric(lineaCantidades[2])+1,
                                 as.numeric(lineaCantidades[3]))
        i<-filaAzufre #vuelvo a posicionarme en el azufre
        #voy hasta el primer Nitrogeno antes o despues del SOO
        while(eliminarEspacios(molecules[i,1])!= "@<TRIPOS>ATOM" && 
              strsplit(eliminarEspacios(molecules[i,1])[2],"")[[1]][1]!="N"){
          i<-i-1
        }
        if(eliminarEspacios(molecules[i,1])== "@<TRIPOS>ATOM"){
          i<-i+1
          while(eliminarEspacios(molecules[i,1])!= "@<TRIPOS>BOND" && 
                strsplit(eliminarEspacios(molecules[i,1])[2],"")[[1]][1]!="N"){
            i<-i+1
          }
        }
        primerNitrogeno<-eliminarEspacios(molecules[i,1])
        #avanzo filas hasta llegar a los bonds
        i<-i+1
        while(eliminarEspacios(molecules[i,1])!="@<TRIPOS>BOND"){
          i<-i+1
        }
        #me ubico en el ultimo atomo antes de los enlaces
        i<-i-1
        #agrego el hidrogeno faltante
        idNuevoHidrogeno<-as.numeric(eliminarEspacios(molecules[i,1])[1])+1
        atomNuevo<-paste("H",idNuevoHidrogeno,sep = "")
        xNuevo<-as.numeric(primerNitrogeno[3])+0.505
        yNuevo<-as.numeric(primerNitrogeno[4])-0.8746
        zNuevo<-as.numeric(primerNitrogeno[5])+0.0003
        filaNueva<-paste(idNuevoHidrogeno,atomNuevo,xNuevo,yNuevo,zNuevo,"H","1","UNK","0.0000")
        ultimaFila<-nrow(molecules)
        molecules<-rbind(molecules,filaNueva)
        molecules<-data.frame(molecules[c(1:i,nrow(molecules),i+1:(ultimaFila-i)),1],stringsAsFactors = FALSE)
        #avanzo filas hasta llegar a SUBSTRUCTURE
        i<-i+2
        while(eliminarEspacios(molecules[i,1])!="@<TRIPOS>SUBSTRUCTURE"){
          i<-i+1
        }
        #me ubico en el ultimo atomo antes de SUBSTRUCTURE
        i<-i-1
        #agrego el enlace faltante
        idNuevoEnlace<-as.numeric(eliminarEspacios(molecules[i,1])[1])+1
        filaNueva<-paste(idNuevoEnlace,primerNitrogeno[1],idNuevoHidrogeno,numEnlaceNuevo)
        ultimaFila<-nrow(molecules)
        molecules<-rbind(molecules,filaNueva)
        molecules<-data.frame(molecules[c(1:i,nrow(molecules),i+1:(ultimaFila-i)),1],stringsAsFactors = FALSE)
      }
    }
    i<-i+1
  }
  return(molecules)
}
