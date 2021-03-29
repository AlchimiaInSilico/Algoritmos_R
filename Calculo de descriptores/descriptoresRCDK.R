#La funcion descriptoresRCDK() realiza el calculo en paralelo de la BD de polimeros que se desea cargar (modificable dentro de la funcion)
#dicha BD debe contener el SMILES de cada polimero y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)
#el calculo de descriptores lo realiza llamando a la funcion calcularDescriptores, la cual utiliza la libreria rcdk

descriptoresRCDK<-function(){
  library(readxl)
  library(foreach)
  library(doParallel)
  
  #Setea lo necesario para el calculo en paralelo
  cores=max(1, detectCores(logical = TRUE)-1)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  try({
	#Se carga la BD de polimeros, junto con la cantidad de URES que corresponde (Mn ó Mw)
    setwd("~/Desktop/Descriptores_RCDK_conBDcorregida2020")
    BD_77polimerosSMILES_URE_CORRECTA_PolyMaS <- read_excel("BD_77polimerosSMILES_URE-CORRECTA_PolyMaS.xlsx", 
                                                            sheet = "Hoja 2") # aqui lee el archivo de SMILES
    
    URES <- read_excel("Base de datos_SMILES_77Pol_10032020.xlsx", 
                         sheet = "Mn_reducción")
    
    polimeros<-cbind(BD_77polimerosSMILES_URE_CORRECTA_PolyMaS,URES)

	#Aca comienza a calcular en paralelo, llamando a la funcion calcularDescriptores(polimeros,i)
    calculados<-data.frame()
    ptime<-system.time({
      calculados<-foreach(i=1:nrow(polimeros), .combine = 'rbindDiferentesColumnas', .export = 'calcularDescriptores') %dopar% {
        if(!(paste("desc77pol_40Mn_polimero_",i,".csv",sep = "") %in% list.files(path = "Calculados")))
          calcularDescriptores(polimeros,i)
      }
    })
    print(ptime)
  })
  stopCluster(cl)
  
  return(calculados)
}

#Realiza el calculo de descriptores utilizando la libreria rcdk. Se le debe pasar por parametro un dataframe de polimeros que contenga al menos el SMILES
# y la cantidad de URES necesarias para alcanzar el peso deseado (Mn, Mw, etc)
calcularDescriptores<-function(polimeros, i){
  source("polimerizadorParalelo.R")
  library(rcdk)
  
  descriptoresMoleculares <- get.desc.names(type="all") 
  descriptoresAtomicos <- get.atomic.desc.names(type="all") 
  #Elimino descriptores que tienen error (con desc tardo 25575.504s > 2UR de 73 a 77  --- sin desc 54s)
  descriptoresMoleculares <- descriptoresMoleculares[-7][-28][-28]
  descriptoresTotales<-c(descriptoresMoleculares, descriptoresAtomicos)

  idPolimero<-matrix(polimeros[i,1])
  unidadRepetitiva<-polimeros[i,4]
  veces<-round(polimeros[i,15],digits = 0)*.4   # multiplico por el % adecuado
  # veces<-polimeros[[i,15]]
  smilesGenerado<-polimerizadorParalelo(unidadRepetitiva, veces)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  smilesGenerado<-sub("*","", smilesGenerado, fixed=TRUE)
  mol<-parse.smiles(smilesGenerado)
  valores<-c(idPolimero)
  tor<-NULL

  for(j in 1:length(descriptoresTotales)) {
    try(valores<-cbind(valores, eval.desc(mol[[1]], descriptoresTotales[j])))
  }
  if(length(valores)>1)
    tor<-valores
  
  write.csv(tor, paste("Calculados/desc77pol_40Mn_polimero_",i,".csv",sep = ""),row.names = F) 
  return(tor)
}

#Une dos filas con diferente numero de columnas
rbindDiferentesColumnas <- function(x, y) {
  if(length(x)>0 && length(y)>0){
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
  }

  return(rbind(x, y))
}

# Estas lineas comentadas son para unir todos los archivos de polimeros que se escribieron por separado.
# comentar y descomentar. Pega archivos. Recordar cambair el nombre al correcto

# polimeros <- c()
# for(i in 1:77){
#   library(readr)
#   try({
#     archivo <- read_csv(paste("Calculados/desc77pol_40Mn_polimero_",i,".csv",sep = ""))
#     polimeros<-rbindDiferentesColumnas(polimeros,archivo)
#   })
# }
# write.csv(polimeros, "Calculados/desc77pol_40Mn.csv",row.names = F)

descriptoresRCDK()