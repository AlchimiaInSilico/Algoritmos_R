#Realiza el calculo de descriptores utilizando la libreria camb (GeneratePadelDescriptors). Se debe setear la BD de polimeros que se desea
#dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)

source("algoritmoPolimerizacion.R")

library(camb)
library(xlsx)
library(gdata)

#Carga la BD de polimeros
polimeros<-read.xlsx("77Polimeros.xlsx",1,stringsAsFactors=FALSE)
descriptor.types <- c("2D")
calculados<-matrix()

idPolimero<-matrix(polimeros[1,2])
unidadRepetitiva<-polimeros[1,5]
veces<-2 #cantidad de UR que tendra la cadena
smilesGenerado<-algoritmoPolimerizacion(unidadRepetitiva, veces)
fileConn<-file("calculoDesdeXls.smi")
writeLines(smilesGenerado, fileConn)
close(fileConn)
descriptors <- GeneratePadelDescriptors(standardised.file = "calculoDesdeXls.smi",
                                        types = descriptor.types, threads = 4)
descriptors<-descriptors[-1]
calculados<-cbindX(idPolimero,descriptors)
colnames(calculados)[1]<-"ID"

for(i in 1:(nrow(polimeros))){
  idPolimero<-matrix(polimeros[i,2])
  unidadRepetitiva<-polimeros[i,5]
  veces<-round(polimeros[i,8],digits = 0)
  smilesGenerado<-algoritmoPolimerizacion(unidadRepetitiva, veces)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  fileConn<-file("calculoDesdeXls.smi")
  writeLines(smilesGenerado, fileConn)
  close(fileConn)
  descriptors <- GeneratePadelDescriptors(standardised.file = "calculoDesdeXls.smi",
                                          types = descriptor.types, threads = 4)
  descriptors<-descriptors[-1]
  calculados[i,]<-cbindX(idPolimero,descriptors)
  print(i)
  print(veces)
}



write.xlsx(calculados, "descriptoresCalculados.xlsx") 