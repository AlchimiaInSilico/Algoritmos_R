#Realiza la polimerizacion comptacional de una Unidad Repetitiva (UR) aprovechando el calculo en paralelo. Se debe ingresar por parametro
# la UR a polimerizar y la cantidad de veces (la cantidad de UR que tendra la cadena resultante)
polimerizadorParalelo<-function(unidadRepetitiva, veces){
  library(foreach)
  library(doParallel)
  
  #Setea lo necesario para el calculo en paralelo
  cores=max(1, detectCores(logical = TRUE)-1)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  #cantidad optima de divisiones del polimero original a los paralelos
  cantidadDivisiones<-ceiling(veces*0.003) 
  #balancear la cantidad de veces por core
  vecesEntero<-veces%/%cantidadDivisiones
  vecesResto<-veces%%cantidadDivisiones
  cantidadVeces<-array(data=vecesEntero,dim=cantidadDivisiones)
  for(i in 1:cantidadDivisiones){
    if(vecesResto>0){
      cantidadVeces[i]=vecesEntero+1
      vecesResto<-vecesResto-1
    }
  }
  
  #polimerizar en paralelo dividiendo por cant de cores
  ptime<-system.time({
    polimeros<-foreach(i=1:cantidadDivisiones, .combine='rbind', .export = 'algoritmoPolimerizacion') %dopar% {
      if(cantidadVeces[i]>0)
        algoritmoPolimerizacion(unidadRepetitiva, cantidadVeces[i])
    }
    
    #el objetivo es obtener una unica cadena representando el polimero completo
    while(length(polimeros)>1){
      #tomo cada uno de los n=cores polimeros y realizo la union en paralelo de a dos polimeros
      #para obtener aproximadamente n/2 polimeros
      polimerosCoresParseados<-foreach(i=seq(from=1, to=length(polimeros), by=2), .combine='rbind', .export = 'unionDosPolimerosEnUno') %dopar% {
        unionDosPolimerosEnUno(polimeros[i], polimeros[i+1])
      }
      polimeros<-polimerosCoresParseados
    }
    
    #en esta instancia voy a tener un unico polimero completo
    polimero<-polimeros
  })
  stopCluster(cl)
  print(ptime)
  return(polimero)
}



algoritmoPolimerizacion <-function(unidadRepetitiva, n){
  if(n>1){
    # se inicializa la variable que contendr? el SMILES resultante
    # se reemplaza el primer * de la unidadRepetitiva por ??? quedando un *
    # restante y se asigna el resultado a la variable smilesGenerado
    smilesGenerado<- sub("*","?", unidadRepetitiva, fixed=TRUE)
    
    # se elimina el primer * de la cadena original reemplaz?ndolo por
    # cadena vac?a ?? y se asigna el resultado en la variable URSinAsterisco
    URSinAsterisco<-sub("*","", unidadRepetitiva, fixed=TRUE)
    
    for(i in 1:(n-1)){
      # se reemplaza el ?nico ?*? presente en ?smilesGenerado? por la cadena 
      # de caracteres almacenada por URSinAsterisco y se asigna el resultado
      # en ?smilesGenerado?
      smilesGenerado<-gsub("*",URSinAsterisco,smilesGenerado, fixed=TRUE)
    }
    
    # se reemplaza el ??? por un ?*?, de esta forma la cadena resultante
    # quedar? con un ?*? para la cabeza y un ?*? para la cola
    smilesGenerado<-sub("?","*",smilesGenerado, fixed=TRUE)
    
    # se retorna el resultado
    
  }
  else{
    smilesGenerado<-unidadRepetitiva
  }
  
  return(smilesGenerado)
}

unionDosPolimerosEnUno<-function(polimero1, polimero2){
  #polimero1 siempre va a ser distinto de NA
  union<-polimero1
  #si polimero2 tambien es distinto de NA, entonces los uno
  if(!is.na(polimero2)){
    smilesGenerado<- sub("*","?", polimero1, fixed=TRUE)
    URSinAsterisco<-sub("*","", polimero2, fixed=TRUE)
    smilesGenerado<-gsub("*",URSinAsterisco,smilesGenerado, fixed=TRUE)
    smilesGenerado<-sub("?","*",smilesGenerado, fixed=TRUE)
    union<-smilesGenerado
  }
  #si polimero2 = NA entonces la union es polimero1
  return(union)
}
