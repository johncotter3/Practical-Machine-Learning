# Load Required Librarys and set a seed for repeatability
library(caret)
library(randomForest)
set.seed(27272)

# Import Data Sets
training=read.csv(file="pml-training.csv",head=TRUE,sep=",",na.strings=c("NA","#DIV/0!",""))
testing=read.csv(file="pml-testing.csv",head=TRUE,sep=",")

# Clean Data by dropping first 7 columns and Removing columns with all NAs and zeros
training<-training[,-seq(1:7)]
testing<-testing[,-seq(1:7)]
hasNA<-as.vector(sapply(training[,1:152],function(x) {length(which(is.na(x)))!=0}))
training<-training[,!hasNA]
testing<-testing[,!hasNA]

# divide training data as training (70%) and testing (30%)
# I will be testing how much accuracy is lost using PCA
inTrain<-createDataPartition(training$classe, p = 0.7)[[1]]
Traintraining<-training[inTrain,]
Traintesting<-training[-inTrain,]

# preprocess with PCA for both training and testing
preProc<-preProcess(Traintraining[,-53],method="pca")
trainPCA<-predict(preProc,Traintraining[,-53])
trainPCA$classe=Traintraining$classe
testPCA<-predict(preProc,Traintesting[,-53])
testPCA$classe=Traintesting$classe

# Train with Random Forests
# Full data 
fitFullRF<-randomForest(Traintraining$classe ~.,data = Traintraining,importance = TRUE)
predictFullRF<-predict(fitFullRF,Traintesting)
fullCM<-confusionMatrix(predictFullRF,Traintesting$classe)

# PCA data
fitpcaRF<-randomForest(trainPCA$classe ~.,data = trainPCA,importance = TRUE)
predictpcaRF<-predict(fitpcaRF,testPCA)
pcaCM <- confusionMatrix(predictpcaRF,testPCA$classe)

# Differences in Accuracy
fullCM$overall[1]-pcaCM$overall[1]

# PCA only loses ~1.8% Accuracy but I want to use the full data anyway
finalRF<-randomForest(training$classe ~.,data = training,importance = TRUE)
Answer<-predict(finalRF,testing)
Answer