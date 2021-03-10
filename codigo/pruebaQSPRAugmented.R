QSPRAugmented<-function(trainAugmented,test){
  options(java.parameters = "-Xmx4096m")
  library(RWeka)
  #creo un clasificador de WEKA con FS incluido
  AttributeSelectedFilter <- make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection")
  algoritmo<-make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
  #Hago el Feature Selection con Wrapper: randomForest, CV10% y BestFirst y creo el modelo
  #con randomForest usando las Features seleccionadas
  print("FS")
  bdFS<-AttributeSelectedFilter(Clase ~ ., trainAugmented[,2:ncol(trainAugmented)],
                                control = Weka_control(
                                  #W = "weka.classifiers.functions.LinearRegression",# -P 100 -I 100 -num-slots 1 -K 0 -M 1.0 -V 0.001 -S 1",
                                  #E = paste("weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.trees.RandomForest -F", numFolds,"-T 0.01 -R 1 -E DEFAULT -- -P 100 -I 100 -num-slots 1 -K 0 -M 1.0 -V 0.001 -S 1"),
                                  E = "weka.attributeSelection.CfsSubsetEval -P 1 -E 1",
                                  S = "weka.attributeSelection.BestFirst -D 1 -N 5") )
  
  print("Creacion Modelo")
  if(ncol(bdFS)>1){
    modelo<-algoritmo(Clase ~ .,bdFS)
    #hago Cross Validation 10% con el modelo (randomForest)
    numFolds <- round(nrow(trainAugmented)*.1)
    CV<-evaluate_Weka_classifier(modelo, numFolds=numFolds)
  }
  
  evaluacionExterna<-predict(modelo,test[,colnames(bdFS)])
  save(list=ls(), file="mylocals.Rda")
  return(evaluacionExterna)  
}

#TrainAugmented<-TrainAugmented[sample(1:241978,24197*4),]
TrainAugmented$Clase<-as.factor(TrainAugmented$Clase)
Test$Clase<-as.factor(Test$Clase)

EE<-QSPRAugmented(TrainAugmented[,-c(1,2)],Test[,-c(1,2)])


