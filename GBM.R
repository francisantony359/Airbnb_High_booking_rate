library("stringr")
library(data.table)
library(glmnet)
library(tree)
library(class)
library(dplyr)
library(plyr)
library(mlbench)
library(caret)
library(randomForest)
library(gbm)
library(neuralnet)
detach("package:dplyr",unload=TRUE)

airbnb<-read.csv("Cleanwithammenities.csv")
airbnb$ID <-NULL
airbnb$review_scores_rating <-NULL

airbnb$high_booking_rate<-as.factor(airbnb$high_booking_rate)

## Making Train and Validation Split

index <- createDataPartition(airbnb$high_booking_rate, p=0.75, list=FALSE)
trainSet <- airbnb[ index,]
testSet <- airbnb[-index,]

#Labling Predictors and Outcome Variable

outcomeName<-'high_booking_rate'
predictors<-names(airbnb)[!names(airbnb) %in% outcomeName]
write.csv(predictors,"list.csv")
save(model_gbm, file = "gbm.rda")
summary(airbnb$review_scores_rating)
str(airbnb)
###### models #######

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
# model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
#model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
#model_mda<-train(trainSet[,predictors],trainSet[,outcomeName],method='mda')
summary(model_gbm)

####### Prediction
predictions_log<-predict.train(object=model_bagg_tree,testSet[,predictors],type="raw")
table(predictions_log)
confusionMatrix_gbm<- confusionMatrix(predictions_log,testSet[,outcomeName])
confusionMatrix_gbm


###### running the best model on test data

x<- predict(model_gbm, newdata = test, type = "raw")
table(x)
write.csv(x,"pred.csv")

plot(model_gbm)

######################BART####################################



