creacionModelos<-function(cantModelos,algoritmoAprendizaje,metodoSampleo){
  
  modelos<-list()
  DBdeModelos.SIDs<-c()
  FSdeModelos<-list()
  #creo un clasificador de WEKA con FS incluido
  AttributeSelectedFilter <- make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection")
  
  ##### SELECCIONAR ALGORITMO #######
  switch(algoritmoAprendizaje,
         "RandomForest" = {algoritmo <- make_Weka_classifier("weka/classifiers/trees/RandomForest")},
         "MultilayerPerceptron" = {algoritmo<-make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")},
         "NaiveBayes" = {algoritmo<-make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")},
         "RandomCommittee" = {algoritmo<-make_Weka_classifier("weka/classifiers/meta/RandomCommittee")},
         print("Algoritmo de Aprendizaje Incorrecto")
         )
  
  
  accuracyModelos<-c()
  for(i in 1:cantModelos){
    print(c("Modelo:", i))
    #sampleo con el metodo correspondiente, junto tanto activos e inactivos
    # en un solo dataset y guardo solo los SID
    
    #### SELECCIONAR SAMPLEO #####
    switch(metodoSampleo,
           "HybridMethodSampling" = {BDSampleada<-HybridMethodSampling(inactivos,nrow(activos))},
           "OneSidedRandomSampling" = {BDSampleada<-OneSidedRandomSampling(inactivos,nrow(activos))},
           "sinSampleo" = {datasetSIDs<-dataBase$SID},
           print("Metodo de Sampleo Incorrecto")
           )

    if(metodoSampleo!="sinSampleo"){
      datasetSIDs<-rbind(activos,BDSampleada)$SID
    }
    
    DBdeModelos.SIDs<-cbind(DBdeModelos.SIDs,datasetSIDs)
    #filtro por compuesto (filas) el archivo de descriptores teniendo en cuenta el SID
    trainSampleado<-dataBase[dataBase$SID%in%datasetSIDs,]

    
    #Hago el Feature Selection con Wrapper: randomForest, CV10% y BestFirst y creo el modelo
    #con randomForest usando las Features seleccionadas
    print("FS")
    numFolds<-10
    bdFS<-AttributeSelectedFilter(Clase ~ ., trainSampleado[,comienzoDescriptoresTrain:(ncol(trainSampleado))],
                                  control = Weka_control(
                                    #W = "weka.classifiers.functions.LinearRegression",# -P 100 -I 100 -num-slots 1 -K 0 -M 1.0 -V 0.001 -S 1",
                                    #E = paste("weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.trees.RandomForest -F", numFolds,"-T 0.01 -R 1 -E DEFAULT -- -P 100 -I 100 -num-slots 1 -K 0 -M 1.0 -V 0.001 -S 1"),
                                    E = "weka.attributeSelection.CfsSubsetEval -P 1 -E 1",
                                    S = "weka.attributeSelection.BestFirst -D 1 -N 5") )
    
    print("Creacion Modelo")
    if(ncol(bdFS)>1){
      numFolds <- round(nrow(bdFS)*.1)
      FSdeModelos<-list.append(FSdeModelos, colnames(bdFS))
      modelo<-algoritmo(Clase ~ .,bdFS)
      #hago Cross Validation 10% con el modelo (randomForest)
      CV<-evaluate_Weka_classifier(modelo, numFolds=numFolds)
      porcentajeCorrClasif<-as.numeric(unlist(CV)[2])
      
      if(porcentajeCorrClasif>umbralCVModelos){
        #guardo una lista de modelos que pasan el umbral
        modelos<-list.append(modelos,modelo)
        #y guardo su accuracy
        accuracyModelos<-cbind(accuracyModelos,(porcentajeCorrClasif/100))
      }
    }
  }
  assign("DBdeModelos.SIDs",DBdeModelos.SIDs,envir = .GlobalEnv)
  assign("FSdeModelos",FSdeModelos,envir = .GlobalEnv)
  assign("modelos",modelos,envir = .GlobalEnv)
  assign("accuracyModelos",accuracyModelos,envir = .GlobalEnv)
}

