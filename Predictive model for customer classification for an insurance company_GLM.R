
#REMOVE ALL OBJECTS AND CLEAN WORKING ENVIRONMENT:
rm(list=ls())
gc(full= TRUE)

#DATA LOADING:
trainset<-"train.csv"
newset<-"test.csv"

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

#+++++++++++++++

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

#+++DATA TRANSFORMATIONS

# # for (j in 1:ncol(base.set)) {
# #   base.set[is.na(base.set[,j]),j]<-mean(base.set[,j],na.rm=TRUE)
# # } #imputacja danych, uzupelnienie brakow srednimi wartosciami
# SCALED_DATA_SET<-scale(base.set);SCALED_DATA_SET
# rows.to.erase<-c()
# for (i in 1:nrow(base.set)) {
#   if (any(abs(SCALED_DATA_SET[i,])>6)) {
#     rows.to.erase<-c(rows.to.erase,i)
#   }

# } #determination of outliers

# FINAL_DATA<-data.frame(SCALED_DATA_SET[-rows.to.erase,]);FINAL_DATA #usuniecie wartosci odstajacych
# base.set<-FINAL_DATA

#####################################

#DATA SET SPLIT ON TRAINING AND TESTING:
SplitDataSet<-function(data.set,training.fraction) {
  random.numbers<-sample.int(nrow(data.set))
  quantiles<-quantile(random.numbers,probs=c(0,training.fraction,1))
  split.labels<-cut(random.numbers,quantiles,include.lowest=T,labels=c("training","test"))
  return(split(data.set,split.labels))
}
split.dataset<-SplitDataSet(base.set,0.6)
train.set<-split.dataset$train
test.set<-split.dataset$test

#BULDING MODEL:
#BirthYear+Sex+LicenseYear+CarBrand+CarType+CarYear+CarEngine+EngineCap+CarValue+AssistanceYears+Accidents
#LicenseYear+CarBrand+CarEngine+EngineCap+CarValue

#->lm
 # model<-lm(NextAccident~CarBrand+CarEngine+EngineCap+CarValue,data=train.set)


############################
#MODEL
#->glm
model.badany <- glm(NextAccident~LicenseYear+CarBrand+CarEngine+EngineCap+CarValue, family=binomial, data=train.set)
model<-step(model.badany,k=log(nrow(train.set)),trace=1)


############################


summary(model)
summary(model$fitted.values)

#CUT-OFF POINT OPTIMISATION:
test.response<-c()
CutOff<-seq(from=0.0,to=1,by=0.02)
incorrect.error<-c()
income<-c()
for(i in 1:length(CutOff)) {
  pred.train<-as.integer(model$fitted.values>=CutOff[i])
  incorrect.error[i]<-sum(abs(pred.train-train.set$NextAccident))/length(train.set$NextAccident)
  income[i]<-length(which(pred.train==0))*1100-length(which((pred.train-train.set$NextAccident)==-1))*5500
}

#CHART OF CLASSIFICATION ERROR
plot(CutOff,incorrect.error,type="l")
MinCutOff.Inc<-which.min(incorrect.error)
points(CutOff[MinCutOff.Inc],incorrect.error[MinCutOff.Inc],col="red",cex=2)
CutOff[MinCutOff.Inc]

#CHART OF REVENUE
barplot(income)
MinCutOff.Income<-which.max(income)
CutOff[MinCutOff.Income]
points(MinCutOff.Income,income[MinCutOff.Income],col="red",cex=2)

#TEST ON TEST SET:
pred.test<-predict(model,newdata=test.set,type="response")
resp.test<-as.integer(pred.test>=CutOff[MinCutOff.Income])
incorrect.test<-sum(abs(resp.test-test.set$NextAccident))/length(test.set$NextAccident)
income.test<-length(which(resp.test==0))*1100-length(which((resp.test-test.set$NextAccident)==-1))*5500
income.test

#DECISION MAKING (FINAL DECISION ON CUT-OFF):
pred.new.set<-predict(model,newdata=new.set,type="response")
resp.test<-as.integer(pred.new.set>=CutOff[MinCutOff.Income])
income.test<-length(which(resp.test==0))*1100-length(which((resp.test-new.set$NextAccident)==-1))*5500
income.test

#SAVING OUTCOME:
write.csv(resp.test,"RESULTS_OF_GLM_MODEL.csv",row.names=FALSE,quote=FALSE)

