setwd("F:/UBIT 7th/data mining/Moddling")
#classification for credit data
library(e1071)
library(ROCR)
library(SDMTools)
library(rpart)
library(randomForest)

#processing data
data<-read.csv("Datatransformed.csv") 
str(data)
data1 <- data[1:1214,]
data<- data1
#Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:1214, 
  size = 607)

# Inspect the random indexes
print(indexes)

# Create a training set from indexes
train <- data[indexes, ]

# Create a test set from remaining indexes
test <- data[-indexes, ]
test[608,] <- NULL
str(data)
test1 <- test[1:607,]
test <- test1

test <- test[!is.na(airquality$Ozone),]

#DECISION-TREE LEARNING#

#learning from training
treemodel<-rpart(as.factor(IsTerrorist)~.,data = train,method="class")

#predicting the test
treemodel.class<-predict(treemodel,test,type="class")
treemodel.probs<-predict(treemodel,test,type="prob")
treemodel.labels<-test$IsTerrorist

#analyzing result
treemodel.confusion<-confusion.matrix(treemodel.labels,treemodel.class)
treemodel.accuracy<-prop.correct(treemodel.confusion)

#roc analysis for test data
treemodel.prediction<-prediction(treemodel.probs[,2],treemodel.labels)
treemodel.performance<-performance(treemodel.prediction,"tpr","fpr")
treemodel.auc<-performance(treemodel.prediction,"auc")@y.values[[1]]

#Rnad
#Formula
myFormual <- IsTerrorist ~ Age + Attended.terrorist.training.camp + Sex +
  Madrasa.training + Educational.attainment + MaritalStatus

#rfmodel<-randomForest(IsTerrorist~.,data = train,importance=TRUE)
rfmodel<-randomForest(myFormual,data = train,importance=TRUE)


#predicting the test
rfmodel.class<-predict(rfmodel,test,type="response")
rfmodel.probs<-predict(rfmodel,test)
rfmodel.labels<-test$IsTerrorist

#analyzing result
rfmodel.confusion<-confusion.matrix(rfmodel.labels,rfmodel.class)
rfmodel.accuracy<-prop.correct(rfmodel.confusion)

library(caret)

#tablr or confuction matrix
# check the prediction
table(predict(Defaulter_ctree), trainData$default.payment.next.month)
print(Defaulter_ctree)

# predict on test data
testPred <- predict(rfmodel.class, newdata = test)
table(testPred, testData$default.payment.next.month)

confusionMatrix(test$IsTerrorist,rfmodel.class)
length(data$IsTerrorist)
length(rfmodel.class)

#roc analysis for test data
rfmodel.prediction<-prediction(rfmodel.probs,rfmodel.labels)
rfmodel.performance<-performance(rfmodel.prediction,"tpr","fpr")
rfmodel.auc<-performance(rfmodel.prediction,"auc")@y.values[[1]]

#COMPARING ROC PLOT of 4 Model#
windows()
plot(treemodel.performance,add=FALSE,col="blue",lwd=2)
plot(rfmodel.performance,add=TRUE,col="black",lwd=2)
