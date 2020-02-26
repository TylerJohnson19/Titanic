library(readr)
library(caTools)
library(ggplot2)
library(pacman)
library(dplyr)
test <- read_csv("Cornell_R/titanic/test.csv")
View(test)
train <- read_csv("Cornell_R/titanic/train.csv")
View(train)
boat <- train
View(boat)

#descriptives
head(boat)
summary(boat)
boat_model <- lm(boat$Age_Avg~boat$Fare)
summary(boat_model)
summary(boat_model)$r.squared

mean(boat$Age,na.rm=TRUE)
boat$Age_Avg <- boat$Age
boat <- within(boat,Age_Avg[is.na(Age)]<-mean(Age,na.rm=TRUE))
boat
cor(boat[c(2,3,7,8,10,13)],boat[c(2,3,7,8,10,13)])
cor(boat$Fare,boat$Age_Avg)

head(test)
summary(test)

Surv_Gen <- aggregate(Survived~Sex,boat,mean)
Surv_Gen
Surv_Age <- aggregate(Age_Avg~Survived,boat,mean)
Surv_Age
Surv_Class <- aggregate(Survived~Pclass,boat,mean)
Surv_Class
Surv_Fare <- aggregate(Fare~Survived,boat,mean)
Surv_Fare

summary(boat$Fare)

#visualizations
boat_Surv <- filter(boat,Survived > 0)
View(boat_Surv)
summary(boat_Surv)

boat_dead <- filter(boat,Survived < 1)
View(boat_dead)
summary(boat_dead)

dead_age_den <- density(boat_dead$Age_Avg)
surv_age_den <- density(boat_Surv$Age_Avg)
plot(surv_age_den,
     main="Density of Ages for Passengers that Survived vs Died",
     xlab="Age",
     col="green",
     ylim = c(0,0.08),
     xlim = c(0,80))
par(new=T)
plot(dead_age_den,
     main="",
     xlab="",
     col="red",
     ylim = c(0,0.08),
     xlim = c(0,80))
par(new=T)
legend("topright",legend = c("Survived", "Died"), fill = c("green","red"), cex = 1.25)

plot(Fare~Age_Avg,data=boat)
abline(lm(boat$Fare~boat$Age_Avg), col="red")

boxplot(boat$Fare~boat$Survived,
        xlab = "Survived",
        ylab = "Fare ($)",
        main = "Survived vs Dead Passengers Compared Against Fare Prices Paid",
        cex.lab = 1.25)

#linear regression and predictions
boat$Survived <- as.factor(boat$Survived)
SurvGLM <- glm(Survived~Age_Avg+Sex+Pclass+Fare, family = binomial(), data = boat)
summary(SurvGLM)

test$Age_Avg <- test$Age
test <- within(test,Age_Avg[is.na(Age)]<-mean(Age,na.rm=TRUE))
test

Surv_Titanic <- predict(SurvGLM, newdata = test, type = "response")
Surv_Titanic
test$Survived_1 <- Surv_Titanic

Surv_Tab<-table(test$Survived,Surv_Titanic>.5)
Surv_Tab

SurvACC <- (Surv_Tab[2,2]+Surv_Tab[1,1])/nrow(boat)
SurvACC

SurvPREC <- (Surv_Tab[2,2]/(Surv_Tab[2,1]+Surv_Tab[2,2]))
SurvPREC

summary(test)
test$Survived = ifelse(test$Survived_1 >0.5, 1, 0)
View(test)
test <- test[,c(-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-14)]
test
write.csv(test, row.names = F, "Titanic_test.csv")
