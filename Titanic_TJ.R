library(readr)
library(caTools)
ERDr <- read_csv("EasyReadersData.csv")
ERDr$Buy<-as.factor(ERDr$Buy)
ERDrGLM<-glm(Buy~Gender+YoungReaders,family=binomial(),data=ERDr)
BuyHat<-predict(ERDrGLM, type="response")
table(ERDr$Buy,BuyHat>.5)
CF<-table(ERDr$Buy,BuyHat>.5)
Accuracy<-(CF[2,2]+CF[1,1])/nrow(ERDr)
Accuracy
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
Precision
Revenue<-within(Revenue,IncNAavg[is.na(Income)]<-mean(Income,na.rm=TRUE))