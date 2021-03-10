library(data.table)
archivos<-list.files()[1:54]
baseDatosLeish<-fread(archivos[1])
 for (i in 2:54){
    baseDatosLeish<-cbind(baseDatosLeish,fread(archivos[i])[,-c(1,2)])
 }