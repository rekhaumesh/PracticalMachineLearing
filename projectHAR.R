## Practical Machine Learning
## Project - Human Activity Recognition - HAR


# Libraries
library(caret)
library(corrplot)
library(kernlab)
library(randomForest)

#Read the training set
raw_har<-read.csv("pml-training.csv",header=TRUE)

# Remove the columns with all/maximum Blanks
# remove columns which contains maximum NAs
raw_har[raw_har==""] <- NA
nonas <- raw_har[, colSums(is.na(raw_har)) == 0]
# Beacuse of the above cleaning, number of variables has been reduced to 60
names(nonas)
nonas<-nonas[,-c(1,2,3,4,5,6)]

# Create data partition - test and train
library(caret)
inTrain <- createDataPartition(y=raw_har$classe,p=0.7,list=FALSE)
train<-nonas[inTrain,]
crossval<-nonas[-inTrain,]
dim(train)
dim(crossval)

# A random forest model was selected to predict the classification
# because it has methods for balancing error in class population 
# unbalanced data sets. The correlation between any two trees 
# in the forest increases the forest error rate. 
# Therefore, a correllation plot was produced in order to see 
# how strong the variables relationships are with each other.
# plot a correlation matrix

corMat <- cor(train[, -length(train)])
corrplot(corMat, order = "FPC", method = "circle", 
        type = "lower", tl.cex = 0.8,  
        tl.col = rgb(0, 0, 0))

## Random Forest model to predict the classe
modFit<-randomForest(classe ~., data=train, mtry=3)
summary(modFit)
plot(modFit,log="y")

# Cross validation of 30% data in the train.csv file

predCrossVal <- predict(modFit, crossval)
confusionMatrix(crossval$classe, predCrossVal)

# Model yeided 99.3% accuracy


###Plotting some graphs for better understanding
par(mfrow=c(1,2))
varImpPlot(modFit,main='Variable Importance Plot: Final Model',pch=16,col='blue')
plot(modFit, main='Error vs No. of trees plot: Final Model',col='blue')

actual<-crossval$classe
result<-data.frame(actual=actual,predicted=predCrossVal)

ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.4)+
  ggtitle('Plotting Error')


##Random Forest using CARET
modCaret<-train(train[,-54],train$classe,tuneGrid=data.frame(mtry=3),trControl=trainControl(method="none"))
summary(modCaret)
plot(varImp(modCaret))
varImp(modCaret)

# Cross validation of 30% data in the train.csv file
predCrossValCar <- predict(modCaret, crossval)
confusionMatrix(crossval$classe, predCrossValCar)


##Predictions
#A separate data set then loaded into R and cleaned the same way.
# predict the classification of the 20 data in the test dataset given
# this has to be submitted for scoring

#Read the training set
test_har<-read.csv("pml-testing.csv",header=TRUE)

# Remove the columns with all/maximum Blanks
# remove columns which contains maximum NAs
test_har[test_har==""] <- NA
test_nonas <- test_har[, colSums(is.na(test_har)) == 0]
# Beacuse of the above cleaning, number of variables has been reduced to 60
names(nonas)
test_nonas<-test_nonas[,-c(1,2,3,4,5,6)]
names(test_nonas)
names(train)

##Predict using RANDOM Forest funcion
predTest <-predict(modFit,test_nonas)
predTest
summary(predTest)

# Predict Using CARET function
predTestCa <-predict(modCaret,test_nonas)
predTestCa
summary(predTestCa)


