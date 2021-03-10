source("corregirNOO.R")
source("corregirSOO.R")

corregirErrores<-function(mol2Entrada, nombreSalida){
  molecules<-read.table(mol2Entrada, sep = "\r",stringsAsFactors = FALSE)
  moleculesNOOCorregido<-corregirNOO(molecules,"N.pl3","O.co2","O.co2","2")
  moleculesNOOySOOCorregido<-corregirSOO(moleculesNOOCorregido,"S.o2","O.2","O.2","1")
  write.table(moleculesNOOySOOCorregido,nombreSalida,quote = FALSE,row.names = FALSE, col.names = FALSE)
}

for(i in 1:8){
  corregirErrores(paste("LeishPARTE",i,".mol2",sep = "")
                  ,paste("LeishPARTE",i,"CORREGIDO.mol2",sep = ""))
}
