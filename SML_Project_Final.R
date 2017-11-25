library(dplyr)
library(glmnet)
library(ROCR)
library(ggplot2)
#Loading RData
load(file = "C:\\Users\\saich\\Desktop\\SMLProject\\Dataset.RData")

#Ratio of classes in datasets
Ratio.Train <- sum(Train.Student.Data$internet=="yes")/nrow(Train.Student.Data)
Ratio.Select <- sum(Select.Student.Data$internet=="yes")/nrow(Select.Student.Data)
Ratio.Test <- sum(Test.Student.Data$internet=="yes")/nrow(Test.Student.Data)

#Checking for null values
any(is.na(Train.Student.Data))
any(is.na(Select.Student.Data))
any(is.na(Test.Student.Data))

#Correlations
cor(Train.Student.Data.Matrix)
  #walc and dalc are correlated
  #Mothers job as teacher and mothers education are correlated
  #G1,G2 and G3 are interrelated
  #Mothers education and fathers education are weakly correlated
  #As per correlation values there are no strong relations other than the grades

#Checking for outliers
boxplot(scale(Train.Student.Data[,names(Train.Student.Data[,sapply(Train.Student.Data, is.numeric)])]),
        las=2)
  #There seems to be an outlier for age which is not alarming
  #There seems to be an outlier for absences but all other predictors are within range so this observation need not be excluded as of now

#Pairwise correlations
pairs(x = Train.Student.Data[,names(Train.Student.Data[,sapply(Train.Student.Data, is.numeric)])])

##############################################################################################################
#Logistic Regression:
##########################################
#Forward subset selection:
#Create a null model, to go forward

forwardsselectionobject <- step(object =  glm(formula = internet ~ 1, data = Train.Student.Data,family = binomial),scope =  ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu 
                                + Mjob + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup + famsup
                                + paid + activities + nursery + higher + romantic + famrel+ freetime + goout + Dalc + 
                                  Walc + health + absences + G1 + G2 + G3 ,direction = "forward",trace = FALSE)

forwardsselectionobject
###########################################
#Backward subset selection:
backwardSelectionObject <- step(glm(formula = internet ~ ., data = Train.Student.Data, family = binomial),
                                direction = "backward", trace=F)
backwardSelectionObject
###########################################
#LASSO subset selection:
cv.glmmod <- cv.glmnet(modelMatrix, y=internet, alpha=1,family="binomial",type.measure = "auc")
best.lambda <- cv.glmmod$lambda.min
glmmod <- glmnet(Train.Student.Data.Matrix[,-43], y=Train.Student.Data$internet, alpha=1,
                 family="binomial",lambda = best.lambda)
#We subset based on the coefficients obtained above
##########################################
#Model evaluation based on Forward subset selection:
predForward <- predict(object = forwardsselectionobject,newdata = Train.Student.Data[,-22],type = "response")
fittedForward <- ifelse(predForward > 0.5,1,0)
accuracyForward <- table(fittedForward, Train.Student.Data[,"internet"])
AccuracyOfForward <- sum(diag(accuracyForward))/sum(accuracyForward)
AccuracyOfForward
Forward.pred = prediction(predForward, Train.Student.Data[,22])
Forward.perf = performance(Forward.pred, "tnr", "npv")
plot(Forward.perf, colorize=T, main="Forward",xlab ="Precision",ylab="True Postive Rate")
##########################################
#Model evaluation based on Backward subset selection:
predBackward <- predict(object = backwardSelectionObject,newdata = Train.Student.Data[,-22],type = "response")
fittedBackward <- ifelse(predBackward > 0.5,1,0)
accuracyBackward <- table(fittedBackward, Train.Student.Data[,"internet"])
AccuracyOfBackward <- sum(diag(accuracyBackward))/sum(accuracyBackward)
AccuracyOfBackward
Backward.pred = prediction(predBackward, Train.Student.Data[,22])
Backward.perf = performance(Backward.pred, "tnr", "npv")
#Backward1.perf = performance(Backward.pred, "tnr", "acc")
plot(Backward.perf, colorize=T, main="Backward",xlab ="Precision",ylab="True Postive Rate")
#plot(Backward1.perf, colorize=T, main="Backward",add=T)
##########################################
#Model evaluation based on LASSO subset selection:
predLASSO <- predict(object = glmmod,newx = Train.Student.Data.Matrix[,-43],type = "response")
fittedLassoResults <- ifelse(predLASSO > 0.5,"yes","no")
accuracyLasso <- table(fittedLassoResults, Train.Student.Data[,"internet"])
AccuracyOfLasso <- sum(diag(accuracyLasso))/sum(accuracyLasso)
AccuracyOfLasso
LASSO.pred = prediction(predLASSO, Train.Student.Data[,22])
LASSO.perf = performance(LASSO.pred, "tnr", "npv")
plot(LASSO.perf, colorize=T, main="LASSO",xlab ="Precision",ylab="True Postive Rate")
###########################################
thresholdValues <- seq(from=0.5, to=0.9, by=0.001)

LogisticStatistics <- sapply(thresholdValues, function(x) {
  cl <- ifelse(predForward < x,"no","yes")
  accuracy <- sum(cl == Train.Student.Data$internet)/nrow(Train.Student.Data)
  precision <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(cl == "no")
  truePositiveRate <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(Train.Student.Data$internet == "no")
  return(c(x,accuracy,precision,truePositiveRate))
})

LogisticStatistics <- as.data.frame(t(LogisticStatistics))
colnames(LogisticStatistics) <- c("threshold","accuracy","precision","truePositiveRate")

ggplot(LogisticStatistics,aes(y=seq(from=0,to=1, by=0.1))) + 
  geom_point(aes(x=threshold,y=accuracy,color="accuracy")) +
  geom_point(aes(x=threshold,y=precision,color="precision")) +
  geom_point(aes(x=threshold,y=truePositiveRate,color="true positive rate"))+
  geom_vline(xintercept = 0.675)

#Forward subset selection best threshold is 0.675.
STHForward  <- LogisticStatistics[LogisticStatistics$threshold == 0.675,]
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
LogisticStatistics <- sapply(thresholdValues, function(x) {
  cl <- ifelse(predBackward < x,"no","yes")
  accuracy <- sum(cl == Train.Student.Data$internet)/nrow(Train.Student.Data)
  precision <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(cl == "no")
  truePositiveRate <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(Train.Student.Data$internet == "no")
  return(c(x,accuracy,precision,truePositiveRate))
})

LogisticStatistics <- as.data.frame(t(LogisticStatistics))
colnames(LogisticStatistics) <- c("threshold","accuracy","precision","truePositiveRate")

ggplot(LogisticStatistics,aes(y=seq(from=0,to=1, by=0.1))) + 
  geom_point(aes(x=threshold,y=accuracy,color="accuracy")) +
  geom_point(aes(x=threshold,y=precision,color="precision")) +
  geom_point(aes(x=threshold,y=truePositiveRate,color="true positive rate")) +
  geom_vline(xintercept = 0.732)
#Backward subset selection best threshold is 0.732
STHBackward  <- LogisticStatistics[LogisticStatistics$threshold == 0.732,]
#################################################
LogisticStatistics <- sapply(thresholdValues, function(x) {
  cl <- ifelse(predLASSO < x,"no","yes")
  accuracy <- sum(cl == Train.Student.Data$internet)/nrow(Train.Student.Data)
  precision <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(cl == "no")
  truePositiveRate <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(Train.Student.Data$internet == "no")
  return(c(x,accuracy,precision,truePositiveRate))
})

LogisticStatistics <- as.data.frame(t(LogisticStatistics))
colnames(LogisticStatistics) <- c("threshold","accuracy","precision","truePositiveRate")

ggplot(LogisticStatistics,aes(y=seq(from=0,to=1, by=0.1))) + 
  geom_point(aes(x=threshold,y=accuracy,color="accuracy")) +
  geom_point(aes(x=threshold,y=precision,color="precision")) +
  geom_point(aes(x=threshold,y=truePositiveRate,color="true positive rate")) +
  geom_vline(xintercept = 0.74)
#LASSO subset selection best threshold is 0.74
STHLASSO  <- LogisticStatistics[LogisticStatistics$threshold == 0.74,]
#@@@
cbind(Model = c("Forward","Backward","LASSO"),rbind(STHForward,STHBackward,STHLASSO))
# Best model based on tradeoff between accuracy, True positive and Precision is step Backward Subset 
STHBackward

#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------



  
  




