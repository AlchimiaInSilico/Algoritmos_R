library(data.table)
library(dplyr)
# 
#  baseDatosLeish<-c()
#  for (i in 1:8){
#    parte<-fread(paste(i,".txt", sep = ""))
#     baseDatosLeish<-rbind(baseDatosLeish,parte)
#  }

leerColumnaDeCSV<-function(archivo, columna){
  fread(archivo, select = columna)
}

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
  if(length(columnasAeliminar)>0){
    dataBase<-dataBase[,-columnasAeliminar]
  }
  return(dataBase)
}

#si queremos quedarnos con mas filas, calculamos un factor para ayudar a las filas
#cuando se tome la decision de eliminar.
calcularFactorAyuda<-function(cantFilas,cantColumnas,priorizarFilas){
  factorAyuda<-1
  if(cantFilas<cantColumnas){
    ifelse(priorizarFilas == 1,{factorAyuda<-cantFilas/cantColumnas},
    ifelse(priorizarFilas == -1,{factorAyuda<-cantColumnas/cantFilas},
                                {factorAyuda<-1}))
  }else{
    ifelse(priorizarFilas == 1,{factorAyuda<-cantColumnas/cantFilas},
    ifelse(priorizarFilas == -1,{factorAyuda<-cantFilas/cantColumnas},
                                {factorAyuda<-1}))
  }
  return(factorAyuda)
}


segundoFiltro<-function(dataBase,priorizarFilas){
  cantFilas<-nrow(dataBase)
  cantColumnas<-ncol(dataBase)
  factorAyuda<-calcularFactorAyuda(cantFilas,cantColumnas,priorizarFilas)
  while(sum(is.na(dataBase))>0 && cantFilas>1 && cantColumnas>1){
    vectorProporcionNAFilas<-c()
    vectorProporcionNAColumnas<-c()
    for(i in 1:cantFilas){
      proporcionesNAFila<-sum(is.na(dataBase[i,]))/cantColumnas*factorAyuda
      vectorProporcionNAFilas<-c(vectorProporcionNAFilas,proporcionesNAFila)
    }
    for(j in 1:cantColumnas){
      proporcionesNAColumna<-sum(is.na(dataBase[,j]))/cantFilas
      vectorProporcionNAColumnas<-c(vectorProporcionNAColumnas,proporcionesNAColumna)
    }
    posicionMaxFilas<-which.max(vectorProporcionNAFilas)
    posicionMaxColumnas<-which.max(vectorProporcionNAColumnas)
    #elimino fila o columna dependiendo quien tenga mayor proporcion de NA
    if(vectorProporcionNAFilas[posicionMaxFilas]>vectorProporcionNAColumnas[posicionMaxColumnas]){
      dataBase<-dataBase[-posicionMaxFilas,]
      cantFilas<-cantFilas-1
    } else {
      dataBase<-dataBase[,-posicionMaxColumnas]
      cantColumnas<-cantColumnas-1
    }
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
  filtro<-grep(mensajeDeError,dataBase$MolID)
  return(dataBase[filtro,])
}

conjuntoFilasSinError<-function(dataBase,mensajeDeError){
  filtro<-grep(mensajeDeError,dataBase$MolID)
  return(dataBase[-filtro,])
}



# filasConError<-conjuntoFilasConError(baseDatosLeish,"error")
# baseDatosLeishSinErrores<-setdiff(baseDatosLeish,filasConError)

#baseDatosLeishFiltrada1<-primerFiltro(baseDatosLeishSinErrores)
#baseDatosLeishFiltrada2<-segundoFiltro(baseDatosLeishFiltrada1)

