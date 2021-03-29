#parametros de entrada:
#unidadRepetitiva: cadena de caracteres correspondiente al codigo SMILES de la Unidad Repetitiva
#n: numero de Unidades Repetitivas que se desean unir.

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