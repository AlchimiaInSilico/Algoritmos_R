library(data.table)
library(dplyr)

 # baseDatosLeish<-c()
 # for (i in 1:8){
 #   parte<-fread(paste(i,".txt", sep = ""))
 #    baseDatosLeish<-rbind(baseDatosLeish,parte)
 # }



baseDatosLeishFiltrada<-primerFiltro(baseDatosLeishSinError)
baseDatosLeishFiltrada<-segundoFiltro(baseDatosLeishFiltrada)

#filtro quitando columnas con valores constantes o error(NA)
primerFiltro<-function(dataBase){
  columnasAeliminar<-c()
  #eliminar columnas con (todo NA) ó (NA y valores constantes)
  for(columna in 1:ncol(dataBase)){
    primerNumero<-buscarPrimerNumero(dataBase,columna)
    cantidadNAoConst<-sum(dataBase[,columna]==primerNumero, na.rm = TRUE) + sum(is.na(dataBase[,columna]))
    if(cantidadNAoConst == nrow(dataBase)){  #si son (todo NA) ó (NA y valores constantes)
      columnasAeliminar<-c(columnasAeliminar,columna)
    }
  }
  return(dataBase[,-columnasAeliminar])
}

segundoFiltro<-function(dataBase){
  cantFilas<-nrow(dataBase)
  cantColumnas<-ncol(dataBase)
  eliminoFila<-0
  i<-1
  while(i<=cantFilas){
    j<-1
    while(j<=cantColumnas && !eliminoFila){
      if(is.na(dataBase[i,j])){
        #calculo las proporciones de NA tanto de la fila como de la columna
        proporcionesNAFila<-sum(is.na(dataBase[i,]))/cantColumnas
        proporcionesNAColumna<-sum(is.na(dataBase[,j]))/cantFilas
        #elimino fila ó columna dependiendo quien tenga mayor proporcion de NA
        if(proporcionesNAFila>proporcionesNAColumna){
          dataBase<-dataBase[-i,]
          i<-i-1
          eliminoFila<-1
        } else {
          dataBase<-dataBase[,-j]
          j<-j-1
        }
      }
      j<-j+1
      cantColumnas<-ncol(dataBase)
    }
    i<-i+1
    cantFilas<-nrow(dataBase)
  }
  return(dataBase)
}


#busca el primer numero que no sea NA, si son todos NA retorno -1
buscarPrimerNumero<-function(dataBase, columna){
  i<-1
  tor<-c()
  while(is.na(dataBase[i,columna]) && i<nrow(dataBase)){
    i<-i+1
  }
  if(!is.na(dataBase[i,columna]))
    tor<-c(tor,dataBase[i,columna])
  else
    tor<-c(tor,-1)
  return(tor)
}

#requisito: la columna donde se encuentren los ids, debe llamarse ID
eliminarFilasPorId<-function(dataBase,listaIDs){
  for(i in 1:length(listaIDs)){
    dataBase<-subset(dataBase,ID!=listaIDs[i])
  }
  return(dataBase)
}

conjuntoFilasConError<-function(dataBase,mensajeDeError){
  return(subset(dataBase,MolID==mensajeDeError))
}


filasConError<-conjuntoFilasConError(baseDatosLeish,"title: molecule 1 - error")
baseDatosLeishSinErrores<-setdiff(baseDatosLeish,filasConError)
