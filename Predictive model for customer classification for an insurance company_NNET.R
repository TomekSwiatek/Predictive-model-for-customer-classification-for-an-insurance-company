
#LOADING LIBRARIES
library(party)
library(rpart)
library(randomForest)
library(RColorBrewer)
library(mgcv)
library(ROCR)
library(Ecdat)

library(nnet)
library(randomForest)

#REMOVE ALL OBJECTS AND CLEAN WORKING ENVIRONMENT:
rm(list=ls())
gc(full= TRUE)

#DATA LOADING:

trainset<-"/train.csv"
newset<-"/test.csv"

base.set<-read.csv(trainset)
new.set<-read.csv(newset)

#DATA INSPECTION AND TRANSFORMATION:

str(base.set)
str(new.set)

dim(base.set)
dim(new.set)

head(base.set)
head(new.set)

summary(base.set)
summary(new.set)

hist(base.set$BirthYear,main="BirthYear",xlab="Year",ylab="")
hist(base.set$Sex,main="Sex",xlab="Sex",ylab="")
hist(base.set$LicenseYear,main="LicenseYear",xlab="Year",ylab="") #year of driving license
hist(base.set$CarYear,main="CarYear",xlab="Year",ylab="") #year of production of the car
hist(base.set$EngineCap,main="EngineCap",xlab="Capacity",ylab="") #engine capacity
hist(base.set$CarEngine,main="CarEngine",xlab="Engine",ylab="") #engine power
hist(base.set$CarValue,main="CarValue",xlab="Value",ylab="") #market value of the car
hist(base.set$AssistanceYears,main="AssistanceYears",xlab="Years",ylab="") #number of years of history insurance
hist(base.set$Accidents,main="Accidents",xlab="Years",ylab="") #the number of accidents / breakdowns in the last five years
table(base.set$Sex)
table(base.set$CarBrand)
table(base.set$CarType)
table(base.set$Accidents)


table(base.set$NextAccident) #the customer caused the damage, target variable

#Car Engine Histogram for BMW
hist(base.set$BirthYear[base.set$CarBrand=="BMW"],main="CarEngine BMW",xlab="BirthYear",ylab="")
#Car Engine Histogram for Toyota
hist(base.set$BirthYear[base.set$CarBrand=="Toyota"],main="BirthYear Toyota",xlab="BirthYear",ylab="")

#Share of accidents for BMW
table(base.set$NextAccident[base.set$CarBrand=="BMW"])[1]/
  sum(table(base.set$NextAccident[base.set$CarBrand=="BMW"]))
#Share of accidents for Toyota
table(base.set$NextAccident[base.set$CarBrand=="Toyota"])[1]/
  sum(table(base.set$NextAccident[base.set$CarBrand=="Toyota"]))

#####################################

#BirthYear+Sex+LicenseYear+CarBrand+CarType+CarYear+CarEngine+EngineCap+CarValue+AssistanceYears+Accidents
#LicenseYear+CarBrand+CarEngine+EngineCap+CarValue

#Share of accidents for CarEngine
table(base.set$NextAccident[base.set$CarEngine>mean(base.set$CarEngine)])[1]/
  sum(table(base.set$NextAccident[base.set$CarEngine>mean(base.set$CarEngine)]))

#Share of accidents for CarEngine
table(base.set$NextAccident[base.set$CarEngine<mean(base.set$CarEngine)])[1]/
  sum(table(base.set$NextAccident[base.set$CarEngine<mean(base.set$CarEngine)]))

#Share of accidents for EngineCap
table(base.set$NextAccident[base.set$EngineCap<mean(base.set$EngineCap)])[1]/
  sum(table(base.set$NextAccident[base.set$EngineCap<mean(base.set$EngineCap)]))

#Share of accidents for CarValue
table(base.set$NextAccident[base.set$CarValue<mean(base.set$CarValue)])[1]/
  sum(table(base.set$NextAccident[base.set$CarValue<mean(base.set$CarValue)]))

#Share of accidents for AssistanceYears
table(base.set$NextAccident[base.set$AssistanceYears<mean(base.set$AssistanceYears)])[1]/
  sum(table(base.set$NextAccident[base.set$AssistanceYears<mean(base.set$AssistanceYears)]))

#Share of accidents for CarEngine
table(base.set$NextAccident[base.set$CarType=="Combi"])[1]/
  sum(table(base.set$NextAccident[base.set$CarType=="Combi"]))

#Histogram for Combi
hist(base.set$CarValue[base.set$CarType=="Combi"],main="CarValue Combi",xlab="Value",ylab="")
#Histogram for Sedan
hist(base.set$CarValue[base.set$CarType=="Sedan"],main="CarValue Sedan",xlab="Value",ylab="")

#Histogram for Combi
hist(base.set$NextAccident[base.set$CarType=="Combi"],main="NextAccident Combi",xlab="Accident",ylab="")
#Histogram for Sedan
hist(base.set$NextAccident[base.set$CarType=="Sedan"],main="NextAccident Sedan",xlab="Accident",ylab="")

summary(base.set$BirthYear)
summary(base.set$Sex)
summary(base.set$CarType)
summary(base.set$CarEngine)

#base.set<-base.set[-1:-2]
#base.set<-base.set[-8:-9]
# base.set<-base.set[-6]


# #DATA TRANSFORMATIONS
# data.set<-base.set
# colMeans(data.set) #srednia wartosc
# sapply(data.set,sd) #odchylenie standardowe
# scaled.data<-scale(data.set[-ncol(data.set)]) #standaryzacja zmiennych bez zmiennej objaœnianej
# base.set<-cbind(scaled.data,data.set[ncol(data.set)])
# dim(base.set)

#####################################

#DATA SET SPLIT ON TRAINING AND TESTING (1 option):
SplitDataSet<-function(data.set,training.fraction) {
  random.numbers<-sample.int(nrow(data.set))
  quantiles<-quantile(random.numbers,probs=c(0,training.fraction,1))
  split.labels<-cut(random.numbers,quantiles,include.lowest=T,labels=c("training","test"))
  return(split(data.set,split.labels))
}
split.dataset<-SplitDataSet(base.set,0.6)
train.set<-split.dataset$train
test.set<-split.dataset$test

#BULDING MODEL
#BirthYear+Sex+LicenseYear+CarBrand+CarType+CarYear+CarEngine+EngineCap+CarValue+AssistanceYears+Accidents
#LicenseYear+CarBrand+CarEngine+EngineCap+CarValue

#->lm
 # model<-lm(NextAccident~LicenseYear+CarBrand+CarEngine+EngineCap+CarValue,data=train.set)


############################

#->NNET MODEL

# RENAMING OF DATASET FOR NNET MODEL
# ttrain.set<-train.set
# valid.set<-test.set

# #DATA SET SPLIT ON TRAINING AND TESTING (2 option):
# training.set.fraction<-0.5
# training.set.index<-(runif(nrow(train.set))<training.set.fraction)
# table(training.set.index)
# ttrain.set<-train.set[training.set.index,]
# valid.set<-train.set[!training.set.index,]
# nrow(ttrain.set)
# nrow(valid.set)


# DEFINITION AND IMPLEMENTATION OD NEURAL NETWORK MODELS
neurons<-5 #number of neurons in the hidden layer
decays<-seq(0,40,length=100) #weight parameter for penalty / regularization factor
wts.parameter<-2*runif(5*21+neurons+1)-1 #initial weight values for the network
train.error<-valid.error<-numeric(length(decays)) #error objects for the test and validation set, respectively
neural.nets<-list()
progress.bar<-winProgressBar("Postep w %","0% zrobione",0,1,0)
for (d in 1:length(decays)) {
  neural.nets[[d]]<-nnet(NextAccident ~ .,data=ttrain.set,size=neurons,decay=decays[d],linout=T,maxit=10000,trace=FALSE,Wts=wts.parameter)
  train.error[d]<-mean(neural.nets[[d]]$residuals^2)
  prediction<-predict(neural.nets[[d]],newdata=valid.set)
  valid.error[d]<-mean((prediction-valid.set$NextAccident)^2)
  percentage<-d/length(decays)
  setWinProgressBar(progress.bar,percentage,"Postep w %",sprintf("%d%% zrobione",round(100*percentage)))
}
close(progress.bar)

## CHOOSING THE BEST NEURAL NETWORK MODEL
model <-neural.nets[[which.min(valid.error)]]


#NEURAL NETWORK STRUCTURE
names(model)
model$n #structure
model$wts #weights in individual neurons
summary(model)

############################

summary(model)

#CUT-OFF POINT OPTIMISATION:
test.response<-c()
CutOff<-seq(from=0.0,to=1,by=0.02)
incorrect.error<-c()
income<-c()
for(i in 1:length(CutOff)) {
  pred.train<-predict(model,newdata=train.set)
  pred.train<-as.integer(pred.train>=CutOff[i])
  incorrect.error[i]<-sum(abs(pred.train-train.set$NextAccident))/length(train.set$NextAccident)
  income[i]<-length(which(pred.train==0))*1100-length(which((pred.train-train.set$NextAccident)==-1))*5500
}
#Chart of Classification Error
#plot(CutOff,incorrect.error,type="l")
MinCutOff.Inc<-which.min(incorrect.error)
points(CutOff[MinCutOff.Inc],incorrect.error[MinCutOff.Inc],col="red",cex=2)
CutOff[MinCutOff.Inc]

#Chart of Revenue
#barplot(income)
MinCutOff.Income<-which.max(income)
CutOff[MinCutOff.Income]
points(MinCutOff.Income,income[MinCutOff.Income],col="red",cex=2)

#TEST ON TEST SET:
pred.test<-predict(model,newdata=test.set)
resp.test<-as.integer(pred.test>=CutOff[MinCutOff.Income])
incorrect.test<-sum(abs(resp.test-test.set$NextAccident))/length(test.set$NextAccident)
income.test<-length(which(resp.test==0))*1100-length(which((resp.test-test.set$NextAccident)==-1))*5500
income.test

#DECISION MAKING (final decision on Cut-Off):
pred.new.set<-predict(model,newdata=new.set)
resp.test<-as.integer(pred.new.set>=CutOff[MinCutOff.Income])
income.test<-length(which(resp.test==0))*1100-length(which((resp.test-new.set$NextAccident)==-1))*5500
income.test

#SAVING OUTCOME:
write.csv(resp.test,"Tomasz Œwi¹tek.csv",row.names=FALSE,quote=FALSE)

