##############################################################################
#Purpose: the R code is used to build regression models for predicting PBPK modeling
#parameters, including Henry’s Law constant (HLC), Fraction unbound to plasma proteins (Fu),
#partition coefficients between plasma and tissues (Plasma-Gut, Plasma-Kidney and liver).

#All models are built using multiple linear regression

##Author Name: Dan Zang, Integrated Laboratory Systems, Inc.

##Author Email: dzang@ils-inc.com

##Date: October, 2017

##############################################################################

#The R data files are stored in the directory C:/PBPKModeling.

setwd("C:/PBPKModeling")

# Henry’s Law constant (HLC)
# Load the data set with 1808 chemicals and 150 fingerprint bits

HenryData <-read.table("HenryConstant-Data.txt", header=T, sep="\t", as.is=T )

# Training set with 1356 chemicals

HenryTraining<-HenryData[1:1356, 1:151]

# Test set with 452 chemicals

HenryTest<-HenryData[1357:1808, 1:151]

# Build multiple linear regression (MLR) models

HenryMLR<-lm(HenryTraining$HenryConstant~., data= HenryTraining)

summary(HenryMLR)

# Predict training Henry constant

HenryMLRTrainingPred<-predict(HenryMLR, HenryTraining)

# Regression between predicted and measured values for training data

RegMLRTrainingHenry<-lm(HenryMLRTrainingPred ~ HenryTraining$HenryConstant)

summary(RegMLRTrainingHenry)

# Predict test Henry constant

HenryMLRTestPred<-predict(HenryMLR, HenryTest)

# Regression between predicted and measured values for test data

RegMLRTestHenry<-lm(HenryMLRTestPred ~ HenryTest$HenryConstant)

summary(RegMLRTestHenry)


#Fraction unbound to plasma proteins (Fu)
# Load the data set with 1538 chemicals and 500 fingerprint bits

FuData <-read.table("Fu-Data.txt", header=T, sep="\t", as.is=T )

# Training set with 1265 chemicals

FuTraining<-FuData[1:1265, 1:501]

# Test set with 273 chemicals

FuTest<-FuData[1266:1538, 1:501]

# Build multiple linear regression (MLR) models

FuMLR<-lm(FuTraining$FUB~., data= FuTraining)

summary(FuMLR)

# Predict training Fu

FuMLRTrainingPred<-predict(FuMLR, FuTraining)

# Set predicted values greater than 1 to 1

FuMLRTrainingPredSetGreater1To1 <-ifelse(FuMLRTrainingPred >1, 1, FuMLRTrainingPred)

# Set predicted values less than 0 to 0

FuMLRTrainingPredSetLess0To0 <-ifelse(FuMLRTrainingPredSetGreater1To1 <0, 0, FuMLRTrainingPredSetGreater1To1)

# Regression between predicted and measured values for training data

RegMLRTrainingFu<-lm(FuMLRTrainingPredSetLess0To0 ~ FuTraining$FUB)

summary(RegMLRTrainingFu)

# Predict test Fu

FuMLRTestPred<-predict(FuMLR, FuTest)

# Set predicted values greater than 1 to 1

FuMLRTestPredSetGreater1To1 <-ifelse(FuMLRTestPred >1, 1, FuMLRTestPred)

# Set predicted values less than 0 to 0

FuMLRTestPredSetLess0To0 <-ifelse(FuMLRTestPredSetGreater1To1 <0, 0, FuMLRTestPredSetGreater1To1)

# Regression between predicted and measured values for test data

RegMLRTestFu<-lm(FuMLRTestPredSetLess0To0 ~ FuTest$FUB)

summary(RegMLRTestFu)


#Partition coefficients between plasma and gut
# Load the data set with 69 chemicals and 20 fingerprint bits

GutData <-read.table("Gut-Data.txt", header=T, sep="\t", as.is=T )

# Training set with 47 chemicals

GutTraining<-GutData[1:47, 1:21]

# Test set with 22 chemicals

GutTest<-GutData[48:69, 1:21]

# Build multiple linear regression (MLR) models

GutMLR<-lm(GutTraining$LogGut~., data= GutTraining)

summary(GutMLR)

# Predict training Gut PC

GutMLRTrainingPred<-predict(GutMLR, GutTraining)

# Regression between predicted and measured values for training data

RegMLRTrainingGut<-lm(GutMLRTrainingPred ~ GutTraining$LogGut)

summary(RegMLRTrainingGut)

# Predict test Gut PC

GutMLRTestPred<-predict(GutMLR, GutTest)

# Regression between predicted and measured values for test data

RegMLRTestGut<-lm(GutMLRTestPred ~ GutTest$LogGut)

summary(RegMLRTestGut)


#Partition coefficients between plasma and kidney
# Load the data set with 90 chemicals and 30 fingerprint bits

KidneyData <-read.table("Kidney-Data.txt", header=T, sep="\t", as.is=T )

# Training set with 68 chemicals

KidneyTraining<-KidneyData[1:68, 1:31]

# Test set with 22 chemicals

KidneyTest<-KidneyData[69:90, 1:31]

# Build multiple linear regression (MLR) models

KidneyMLR<-lm(KidneyTraining$LogKidney~., data= KidneyTraining)

summary(KidneyMLR)

# Predict training Kidney PC

KidneyMLRTrainingPred<-predict(KidneyMLR, KidneyTraining)

# Regression between predicted and measured values for training data

RegMLRTrainingKidney<-lm(KidneyMLRTrainingPred ~ KidneyTraining$LogKidney)

summary(RegMLRTrainingKidney)

# Predict test Kidney PC

KidneyMLRTestPred<-predict(KidneyMLR, KidneyTest)

# Regression between predicted and measured values for test data

RegMLRTestKidney<-lm(KidneyMLRTestPred ~ KidneyTest$LogKidney)

summary(RegMLRTestKidney)


#Partition coefficients between plasma and liver
# Load the data set with 88 chemicals and 25 fingerprint bits

LiverData <-read.table("Liver-Data.txt", header=T, sep="\t", as.is=T )

# Training set with 66 chemicals

LiverTraining<-LiverData[1:66, 1:26]

# Test set with 22 chemicals

LiverTest<-LiverData[67:88, 1:26]

# Build multiple linear regression (MLR) models

LiverMLR<-lm(LiverTraining$LogLiver~., data= LiverTraining)

summary(LiverMLR)

# Predict training Liver PC

LiverMLRTrainingPred<-predict(LiverMLR, LiverTraining)

# Regression between predicted and measured values for training data

RegMLRTrainingLiver<-lm(LiverMLRTrainingPred ~ LiverTraining$LogLiver)

summary(RegMLRTrainingLiver)

# Predict test Liver PC

LiverMLRTestPred<-predict(LiverMLR, LiverTest)

# Regression between predicted and measured values for test data

RegMLRTestLiver<-lm(LiverMLRTestPred ~ LiverTest$LogLiver)

summary(RegMLRTestLiver)
