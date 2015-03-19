#Quiz 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength,
                            p=3/4)[[1]]
training = mixtures[inTrain,]
testing = mixtures[-inTrain,]

qplot(CompressiveStrength,Cement,data=training,colour=FineAggregate)
qplot(CompressiveStrength,Cement,data=training,colour=FlyAsh)
qplot(CompressiveStrength,Age,data=training,colour=FlyAsh)
qplot(CompressiveStrength,Age,data=training,colour=Water)
qplot(CompressiveStrength,Age,data=training,colour=Cement)
qplot(CompressiveStrength,Cement,data=training,colour=Water)
qplot(CompressiveStrength,Cement,data=training,colour=Superplasticizer)
qplot(CompressiveStrength,Age,data=training,colour=Water)

#Question 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain=createDataPartition(mixtures$CompressiveStrength,
                            p=3/4)[[1]]
training = mixtures[inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))

#Quiz 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3443)
data(AlzheimerDisease)
adData=data.frame(diagnosis,predictors)
inTrain=createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing=adData[-inTrain,]
IL_variable<-grep("^IL",names(training),value=TRUE)
preProc<-preProcess(training[IL_variable],method="pca",threshold=0.9)
preProc

##Question 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(1203)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain<-createDataPartition(adData$diagnosis,p=3/4)[[1]]
training<-adData[inTrain,]
testing<-adData[-inTrain,]
set.seed(2033)
#Grep for the predictors starting with IL only
IL_variable<-grep("^IL",colnames(training),value=TRUE)
#subset of IL predictors
predictors_IL<-predictors[,IL_variable]
df_IL<-data.frame(diagnosis,predictors_IL)

#Create Data Partition for the IL variables only
inTrain_IL=createDataPartition(df_IL$diagnosis,p=3/4)[[1]]
training_IL<-df_IL[inTrain_IL,]
testing_IL<-df_IL[-inTrain,]

# Model without PCA
modNoPCA<-train(diagnosis~.,method="glm",data=training_IL)
predNoPCA<-predict(modNoPCA,newdata=testing_IL)
C1<-confusionMatrix(predNoPCA,testing_IL$diagnosis)
A1<-C1$overall[1]
print(A1)
#Accuracy 0.70737171

#Model with PCA
predPCA<- train(training_IL$diagnosis ~ ., 
                method = "glm", 
                data = training_IL,
                preProcess = "pca", 
                Control = trainControl(preProcOptions = list(thresh = 0.8)))


C2 <- confusionMatrix(testing_IL$diagnosis,predict(modPCA,testing_IL))
A2<-C2$overall[1]
print(A2)
print(A1)

