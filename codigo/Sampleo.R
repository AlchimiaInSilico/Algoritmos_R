library(readxl)
library(caTools)
library(dplyr)
library(rlist)


reducirSubset<-function(subsetAReducir, ratio){
  train_rows <- sample.split(1:nrow(subsetAReducir), SplitRatio=ratio)
  miniSet <- subsetAReducir[train_rows,]
  return(miniSet)
}


#selecciona random de la clase mayoritaria hasta igualar la minoritaria
OneSidedRandomSampling<-function(setMayoritario, dimensionMinoritaria){
  ratio<-dimensionMinoritaria/nrow(setMayoritario)
  setReducido<-reducirSubset(setMayoritario,ratio)
  return(setReducido)
}

#selecciona random de la clase mayoritaria hasta ser 3 veces la minoritaria
HybridMethodSampling<-function(setMayoritario, dimensionMinoritaria){
  ratio<-3*dimensionMinoritaria/nrow(setMayoritario)
  setReducido<-reducirSubset(setMayoritario,ratio)
  return(setReducido)
}

# repite OneSidedRandom multiples veces hasta que todos los compuestos de la
# clase mayoritaria hayan formado parte del cto de entrenamiento al menos 1 vez
MultipleUnderSampling<-function(setMayoritario, dimensionMinoritaria){
  setMayoritarioOriginal<-setMayoritario
  toReturn<-list()
  while(nrow(setMayoritario)>=dimensionMinoritaria){
    setReducido<-OneSidedRandomSampling(setMayoritario, dimensionMinoritaria)
    toReturn<-list.append(toReturn,setReducido)
    #elimino del set1 las filas que se encuentren en set2 y lo guardo en setMayoritario
    setMayoritario<-setdiff(setMayoritario, setReducido)
  }
  #si la ultima vez la reduccion queda un set >0 (será menor que dimensionMinoritaria)
  #debo restar la mayoritaria - ultimaReduccion y seleccionar random para los faltantes
  if(nrow(setMayoritario)>0){
    #guardo en setReducido los que quedaron fuera del ultimo OneSidedRandomSampling
    setReducido<-setMayoritario
    setMayoritario<-setdiff(setMayoritarioOriginal, setReducido)
    #hago la union del setReducido con los que selecciona aleatoriamente
    #de la diferencia del conjunto original y el setReducido
    setReducido<-rbind(setReducido,OneSidedRandomSampling
                       (setMayoritario, dimensionMinoritaria-nrow(setReducido)))
    toReturn<-list.append(toReturn,setReducido)
  }
}

