library(readr)
library(caTools)
library(FNN)
library(tree)
library(readr)
library(neuralnet)
library(dplyr)
library(pacman)
library(kernlab)
library(FNN)
library(caret)
library(e1071)

boat <- read_csv("Cornell_R/titanic/train.csv")
View(boat)
test <- read_csv("Cornell_R/titanic/test.csv")
View(test)

#make characteristic variables numeric for model
boat$Survived <- as.factor(boat$Survived)
boat$Embarked_Bin <- replace(boat$Embarked, boat$Embarked == "S",1)
boat$Embarked_Bin_S <- replace(boat$Embarked_Bin, boat$Embarked == "C",2)
boat$Embarked_Bin <- replace(boat$Embarked_Bin_S, boat$Embarked == "Q",3)

boat$Embarked_Bin <- as.numeric(boat$Embarked_Bin)

test$Embarked_Bin <- replace(test$Embarked, test$Embarked == "S",1)
test$Embarked_Bin_S <- replace(test$Embarked_Bin, test$Embarked == "C",2)
test$Embarked_Bin <- replace(test$Embarked_Bin_S, test$Embarked == "Q",3)

test$Embarked_Bin <- as.numeric(test$Embarked_Bin)

boat$Sex_Bin <- ifelse(boat$Sex == "male",1,0)
test$Sex_Bin <- ifelse(test$Sex == "male",1,0)

summary(test)
summary(boat)

#replace NAs in Age and Fare
mean(boat$Age,na.rm=TRUE)
boat$Age_Avg <- boat$Age
boat <- within(boat,Age_Avg[is.na(Age)]<-mean(Age,na.rm=TRUE))
boat

mean(boat$Embarked_Bin,na.rm=TRUE)
boat <- within(boat,Embarked_Bin[is.na(Embarked_Bin)]<-mean(Embarked_Bin,na.rm=TRUE))
boat

mean(test$Age,na.rm=TRUE)
test$Age_Avg <- test$Age
test <- within(test,Age_Avg[is.na(Age)]<-mean(Age,na.rm=TRUE))
test

mean(test$Fare,na.rm=TRUE)
test$Fare_Avg <- test$Fare
test <- within(test,Fare_Avg[is.na(Fare)]<-mean(Fare,na.rm=TRUE))
test

boat$Fare_Avg <- boat$Fare

#caret Model knn
set.seed(123)

Model_train <- train(Survived ~ Age_Avg + Fare_Avg + Embarked_Bin + Pclass + SibSp + Parch + Sex_Bin, data = boat, method = "knn")
Model_train

Model_train$finalModel

table(predict(Model_train,boat),boat$Survived)
trControlOptions<-trainControl(method="cv",number=10,selectionFunction ="best")

Model<-train(Survived ~ Age_Avg + Fare_Avg + Embarked_Bin + Pclass + SibSp + Parch + Sex_Bin,data=boat,method="rpart",trControl=trControlOptions,metric="Kappa")
table(predict(Model,boat),boat$Survived)
Model

test$Survived <- predict(Model,test)
test_knn <- test

#caret Model lm
set.seed(123)

Model_train_lm <- train(Survived ~ Age_Avg + Fare_Avg + Embarked_Bin + Pclass + SibSp + Parch + Sex_Bin, data = boat, method = "glm")
Model_train_lm

Model_train_lm$finalModel

table(predict(Model_train_lm,boat),boat$Survived)
trControlOptions<-trainControl(method="cv",number=10,selectionFunction ="best")

Model_lm<-train(Survived ~ Age_Avg + Fare_Avg + Embarked_Bin + Pclass + SibSp + Parch + Sex_Bin,data=boat,method="rpart",trControl=trControlOptions,metric="Kappa")
table(predict(Model_lm,boat),boat$Survived)
Model_lm

test$Survived <- predict(Model_lm,test)
test_glm <- test

test_knn <- test_knn[c(1,17)]
test_glm <- test_glm[c(1,17)]

write.csv(test_knn, row.names = F, "knn_submission.csv")
write.csv(test_glm, row.names = F, "glm_submission.csv")
