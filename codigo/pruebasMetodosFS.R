library(readr)
library(Boruta)
library(randomForest)
library(caret)

#cargo la BD
DB <- read_csv("C:/Users/San/Google Drive/Algoritmo para Imbalanced DB/Prueba con DB TODESCHINI/Con DB DRAGON/Todeschini-DataSet-FIltrado.csv")[,-1]
Y<-as.factor(DB$Clase) 

# Perform Boruta search
tiempoBoruta<-system.time({
  boruta_output <- Boruta(DB[,-2161], Y)
  roughFixMod <- TentativeRoughFix(boruta_output)
  boruta_signif <- getSelectedAttributes(roughFixMod)
})
rfBoruta<-randomForest(DB[,getSelectedAttributes(roughFixMod)],Y)
confusionBoruta<-rfBoruta$confusion


#Genetic Algorithm
# Define control function
tiempoGA<-system.time({
  ga_ctrl <- gafsControl(functions = rfGA, 
                         method = "cv",
                         repeats = 3)
  
  # Genetic Algorithm feature selection
  set.seed(100)
  ga_obj <- gafs(x=DB[,-2161], 
                 y=Y, 
                 iters = 100,   # normally much higher (100+)
                 gafsControl = ga_ctrl)
  
})

#Simulated Annealing
tiempoSA<-system.time({
sa_ctrl <- safsControl(functions = rfSA,
                       method = "repeatedcv",
                       repeats = 3,
                       improve = 5) # n iterations without improvement before a reset

set.seed(100)
sa_obj <- safs(x=DB[,-2161], 
               y=Y, 
               safsControl = sa_ctrl)
})
rfSA<-randomForest(DB[,sa_obj$optVariables],Y)

#Recursive Feature Elimination
tiempoRFE<-system.time({
set.seed(100)
options(warn=-1)

subsets <- c(50)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile50 <- rfe(x=DB[,-2161], 
                 y=Y, 
                 sizes = subsets,
                 rfeControl = ctrl)
})
rfRFE50<-randomForest(DB[,lmProfile50$optVariables],Y)
