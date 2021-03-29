#Realiza el calculo de descriptores utilizando la libreria rcdk. Se debe setear la BD de polimeros que se desea
#dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)

source("algoritmoPolimerizacion.R")

library(rcdk)
library(xlsx)
library(gdata)

#polimeros<-read.xlsx("77Polimeros_fc.xlsx",1,stringsAsFactors=FALSE)
polimeros<-X77Polimeros_fc_REESCALDO

ptime<-system.time({
  descriptoresMoleculares <- get.desc.names(type="all") 
  descriptoresAtomicos <- get.atomic.desc.names(type="all") 
  #Elimino descriptores que tienen error
  descriptoresMoleculares <- descriptoresMoleculares[-7][-28][-28]

calculados<-data.frame()

for(i in 1:(nrow(polimeros))){
  idPolimero<-matrix(polimeros[i,1])
  unidadRepetitiva<-polimeros[i,3]
  veces<-round(polimeros[i,7],digits = 0)
  smilesGenerado<-algoritmoPolimerizacion(unidadRepetitiva, veces)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  mol<-parse.smiles(smilesGenerado)
  valores<-c(idPolimero)
  
  for (j in 1:length(descriptoresMoleculares)) {
    try(
      valores <- cbind(valores, eval.desc(mol[[1]], descriptoresMoleculares[j]))
    )
    print(j)
  }
  for (j in 1:length(descriptoresAtomicos)) {
    try(
      valores <- cbind(valores, eval.desc(mol[[1]], descriptoresAtomicos[j]))
    )
    print(j)
  }
  try(
    calculados<-rbind(calculados,valores)
  )

}


})
print(ptime)

colnames(calculados)[1]<-"ID"
write.xlsx(calculados, "DMs77PolimerosRCDKLogMn.xlsx") 