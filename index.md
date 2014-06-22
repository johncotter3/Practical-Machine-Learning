---
title       : Practical Machine Learning Course Project
subtitle    : Johns Hopkins University
author      : John Cotter
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
---

## Introduction
Background from Assignment
================

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

--- .class #id 

## Test Setup
# Load Required Librarys and set a seed for repeatability

```r
library(caret)
library(randomForest)
set.seed(27272)
```

# Import Data Sets

```r
training=read.csv(file="../pml-training.csv",head=TRUE,sep=",",na.strings=c("NA","#DIV/0!",""))
testing=read.csv(file="../pml-testing.csv",head=TRUE,sep=",")
```

# Clean Data by dropping first 7 columns and Removing columns with all NAs and zeros

```r
training<-training[,-seq(1:7)]
testing<-testing[,-seq(1:7)]
hasNA<-as.vector(sapply(training[,1:152],function(x) {length(which(is.na(x)))!=0}))
training<-training[,!hasNA]
testing<-testing[,!hasNA]
```

--- .class #id

## Test Setup
# Divide training data as training (70%) and testing (30%)
# I will be testing how much accuracy is lost using PCA

```r
inTrain<-createDataPartition(training$classe, p = 0.7)[[1]]
Traintraining<-training[inTrain,]
Traintesting<-training[-inTrain,]
```

# Preprocess with PCA for both training and testing

```r
preProc<-preProcess(Traintraining[,-53],method="pca")
trainPCA<-predict(preProc,Traintraining[,-53])
trainPCA$classe=Traintraining$classe
testPCA<-predict(preProc,Traintesting[,-53])
testPCA$classe=Traintesting$classe
```

--- .class #id

## Training with Random Forests
# Full data 

```r
fitFullRF<-randomForest(Traintraining$classe ~.,data = Traintraining,importance = TRUE)
predictFullRF<-predict(fitFullRF,Traintesting)
fullCM<-confusionMatrix(predictFullRF,Traintesting$classe)
fullCM$overall
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##         0.9941         0.9925         0.9917         0.9959         0.2845 
## AccuracyPValue  McnemarPValue 
##         0.0000            NaN
```

--- .class #id

## Training with Random Forests
# PCA data

```r
fitpcaRF<-randomForest(trainPCA$classe ~.,data = trainPCA,importance = TRUE)
predictpcaRF<-predict(fitpcaRF,testPCA)
pcaCM <- confusionMatrix(predictpcaRF,testPCA$classe)
pcaCM$overall
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      9.752e-01      9.686e-01      9.709e-01      9.790e-01      2.845e-01 
## AccuracyPValue  McnemarPValue 
##      0.000e+00      8.199e-06
```

--- .class #id

## Training Error
In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. It is estimated internally during the run.  However, the error does decrease with the number of trees. The following plot shows the training error vs # of trees. 


```r
plot(fitFullRF,main="Error vs # of trees")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

--- .class #id

## Final Outcomes
# Differences in Accuracy

```r
fullCM$overall[1]-pcaCM$overall[1]
```

```
## Accuracy 
##  0.01886
```

# PCA only loses ~1.8% Accuracy but I want to use the full data anyway

```r
finalRF<-randomForest(training$classe ~.,data = training,importance = TRUE)
Answer<-predict(finalRF,testing)
Answer
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
