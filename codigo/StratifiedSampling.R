source("http://bioconductor.org/biocLite.R") # Sources the biocLite.R installation script. 
biocLite("ChemmineR")
biocLite("ChemmineOB")
library(caTools)
library(ChemmineR)

Class_I <- read.csv('QSAR_Leish_0_I.csv',header=TRUE)
Class_I <- as.matrix(Class_I)
rownames(Class_I) <- c(1:dim(Class_I)[1])
coln <- colnames(Class_I)
Class_I <- cbind(rownames(Class_I),Class_I)
colnames(Class_I) <- c("Original Pos",coln)
train_rows = sample.split(Class_I[,4], SplitRatio=0.041461)
SelMol = Class_I[ train_rows,]
write.csv(SelMol,'QSAR_Leish_0_I_SelMol.csv',row.names=FALSE)

Class_MI <- read.csv('QSAR_Leish_30-5_MI.csv',header=TRUE)
Class_MI <- as.matrix(Class_MI)
rownames(Class_MI) <- c(1:dim(Class_MI)[1])
coln <- colnames(Class_MI)
Class_MI <- cbind(rownames(Class_MI),Class_MI)
colnames(Class_MI) <- c("Original Pos",coln)
train_rows = sample.split(Class_MI[,4], SplitRatio=0.100794)
SelMol = Class_MI[ train_rows,]
write.csv(SelMol,'QSAR_Leish_30-5_MI_SelMol.csv',row.names=FALSE)

Class_GR <- read.csv('QSAR_Leish_60-30_GR.csv',header=TRUE)
Class_GR <- as.matrix(Class_GR)
rownames(Class_GR) <- c(1:dim(Class_GR)[1])
coln <- colnames(Class_GR)
Class_GR <- cbind(rownames(Class_GR),Class_GR)
colnames(Class_GR) <- c("Original Pos",coln)
train_rows = sample.split(Class_GR[,4], SplitRatio=0.440567)
SelMol = Class_GR[ train_rows,]
write.csv(SelMol,'QSAR_Leish_60-30_GR_SelMol.csv',row.names=FALSE)

Class_MA <- read.csv('QSAR_Leish_85-60_MA.csv',header=TRUE)
Class_MA <- as.matrix(Class_MA)
rownames(Class_MA) <- c(1:dim(Class_MA)[1])
coln <- colnames(Class_MA)
Class_MA <- cbind(rownames(Class_MA),Class_MA)
colnames(Class_MA) <- c("Original Pos",coln)
train_rows = sample.split(Class_MA[,4], SplitRatio=0.884017)
SelMol = Class_MA[ train_rows,]
write.csv(SelMol,'QSAR_Leish_85-60_MA_SelMol.csv',row.names=FALSE)

Class_A <- read.csv('QSAR_Leish_100-85_A.csv',header=TRUE)
Class_A <- as.matrix(Class_A)
rownames(Class_A) <- c(1:dim(Class_A)[1])
coln <- colnames(Class_A)
Class_A <- cbind(rownames(Class_A),Class_A)
colnames(Class_A) <- c("Original Pos",coln)
train_rows = sample.split(Class_A[,4], SplitRatio=0.557724)
SelMol = Class_A[ train_rows,]
write.csv(SelMol,'QSAR_Leish_100-85_A_SelMol.csv',row.names=FALSE)

