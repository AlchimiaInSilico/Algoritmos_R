library(xlsx)
library(gdata)

descriptores<-read.xlsx("78Descriptores.xlsx",1,stringsAsFactors=FALSE)
polimeros<-read.xlsx("descriptoresCalculados33Polimeros12PorcDeMw.xlsx",1,stringsAsFactors=FALSE)

calculados<-cbind()
for(i in 1:(ncol(descriptores))){
  #match retorna la posicion de un string dentro de un array u objeto
  posicion<-match(descriptores[1,i], colnames(polimeros))
  calculados<-cbind(calculados,polimeros[,posicion])
}

colnames(calculados)<-descriptores
write.xlsx(calculados, "filtradosMw.xlsx") 