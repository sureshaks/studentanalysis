library(dplyr)
library(glmnet)
library(ROCR)
library(ggplot2)
library(MASS)
library(e1071)
library(caret)
library(tree)
library(randomForest)
library(adabag)

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
cv.glmmod <- cv.glmnet(Train.Student.Data.Matrix[,-43], y=Train.Student.Data$internet, alpha=1,family="binomial")

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



threshold <- seq(from=0.5, to=0.1, by=-0.001)

########### LDA ###########

lda.fit <- lda(internet ~ ., data = as.data.frame(Train.Student.Data))

# Training set Plot LDA ROC curve and LDA accuracy
lda.pr <- predict(lda.fit, Train.Student.Data[,-22])
lda.pred = prediction(lda.pr$posterior[,2], Train.Student.Data[,22])
lda.perf = performance(lda.pred, "tnr", "npv")
plot(lda.perf, colorize=T, main="LDA",xlab ="Precision",ylab="True Postive Rate")
lda.acc <- sum(lda.pr$class == Train.Student.Data[,22])/nrow(Train.Student.Data)
lda.area <- unlist(attributes(performance(lda.pred, "auc"))$y.values)


LdaStatistics <- sapply(threshold, function(x) {
  cl <- ifelse(lda.pr$posterior[,1] > x,"no","yes")
  accuracy <- sum(cl == Train.Student.Data$internet)/nrow(Train.Student.Data)
  precision <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(cl == "no")
  truePositiveRate <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(Train.Student.Data$internet == "no")
  return(c(x,accuracy,precision,truePositiveRate))
})

LdaStatistics <- as.data.frame(t(LdaStatistics))
colnames(LdaStatistics) <- c("threshold","accuracy","precision","truePositiveRate")

ggplot(LdaStatistics,aes(y=seq(from=0,to=1, by=0.1))) + 
  geom_point(aes(x=threshold,y=accuracy,color="accuracy")) +
  geom_point(aes(x=threshold,y=precision,color="precision")) +
  geom_point(aes(x=threshold,y=truePositiveRate,color="true positive rate")) +
  geom_vline(xintercept = 0.255)

#LDA threshold fpr "no" is 0.255
STHLDA  <- LdaStatistics[LdaStatistics$threshold == 0.255,]

########### Naive Bayes ###########

nb.fit <- naiveBayes(internet ~ ., data = as.data.frame(Train.Student.Data))

# Training accuracy and ROC curve
nb.pr = list()
nb.pr$raw <- predict(nb.fit, Train.Student.Data[,-22], type="raw")
nb.pr$val <- predict(nb.fit, Train.Student.Data[,-22])
nb.pred <- prediction(nb.pr$raw[,2], Train.Student.Data[,22])
nb.perf <- performance(nb.pred, "tnr", "npv")
plot(nb.perf, colorize=T, main="Naive Bayes")
nb.acc <- sum(nb.pr$val == Train.Student.Data[,22])/nrow(Train.Student.Data)
nb.area <- unlist(attributes(performance(nb.pred, "auc"))$y.values)

NbStatistics <- sapply(threshold, function(x) {
  cl <- ifelse(nb.pr$raw[,1] > x,"no","yes")
  accuracy <- sum(cl == Train.Student.Data$internet)/nrow(Train.Student.Data)
  precision <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(cl == "no")
  truePositiveRate <- sum(cl == Train.Student.Data$internet & cl=="no")/sum(Train.Student.Data$internet == "no")
  return(c(x,accuracy,precision,truePositiveRate))
})

NbStatistics <- as.data.frame(t(NbStatistics))
colnames(NbStatistics) <- c("threshold","accuracy","precision","truePositiveRate")

ggplot(NbStatistics,aes(y=seq(from=0,to=1, by=0.1))) + 
  geom_point(aes(x=threshold,y=accuracy,color="accuracy")) +
  geom_point(aes(x=threshold,y=precision,color="precision")) +
  geom_point(aes(x=threshold,y=truePositiveRate,color="true positive rate")) +
  geom_vline(xintercept = 0.37)
  
#Naive threshold for "no" is 0.37
STHNB  <- NbStatistics[NbStatistics$threshold == 0.37,]


fittednb  <- ifelse(nb.pr$raw[,1] > STHNB$threshold,"no","yes")
accuracynb <- table(fittednb, Train.Student.Data[,"internet"])
accuracynb

#-------------------------------------------------------------------------------------------
  #TREES
#-----------------------------------------------------------------------------------------

#implementation of a TREE

tree <- tree(internet~.,Train.Student.Data)
plot(tree)
text(tree)
prd.tree <- predict(tree,newdata =  Train.Student.Data,type="class")
table.tree <- table(prd.tree,Train.Student.Data$internet)
(table.tree[1,1]+table.tree[2,2])/nrow(Train.Student.Data)




# TREE by training on increased data for favourable set
train_data_no <- Train.Student.Data[Train.Student.Data$internet=="no",]
Train.Student.Data.biased <- rbind(Train.Student.Data,train_data_no)
biased.tree <- tree(internet~.,Train.Student.Data.biased)
plot(biased.tree)
text(biased.tree)
prd.biased.tree <- predict(biased.tree,newdata =  Train.Student.Data,type="class")
table.biased.tree <- table(prd.biased.tree,Train.Student.Data$internet)




#implementation of a Bagging
bagging50 <- bagging(internet~.,data=Train.Student.Data,mfinal=50)
importanceplot(bagging50,las=2)
prd.bagging50 <- predict(bagging50,newdata=Train.Student.Data)
table.bagging50 <- prd.bagging50$confusion
(table.bagging50[1,1]+table.bagging50[2,2])/nrow(Train.Student.Data)


bagging100 <- bagging(internet~.,data=Train.Student.Data,mfinal=100)
importanceplot(bagging100,las=2)
prd.bagging100 <- predict(bagging100,newdata=Train.Student.Data)
table.bagging100 <- prd.bagging100$confusion
(table.bagging100[1,1]+table.bagging100[2,2])/nrow(Train.Student.Data)

bagging10 <- bagging(internet~.,data=Train.Student.Data,mfinal=10)
importanceplot(bagging10,las=2)
prd.bagging10 <- predict(bagging50,newdata=Train.Student.Data)
table.bagging10 <- prd.bagging10$confusion
(table.bagging10[1,1]+table.bagging50[2,2])/nrow(Train.Student.Data)

bagging500 <- bagging(internet~.,data=Train.Student.Data,mfinal=500)
importanceplot(bagging500,las=2)
prd.bagging500 <- predict(bagging500,newdata=Train.Student.Data)
table.bagging500 <- prd.bagging500$confusion
(table.bagging500[1,1]+table.bagging500[2,2])/nrow(Train.Student.Data)







#implementation of a Random Forest
tcontrol = trainControl(method="repeatedcv",number=5,repeats=5)
rf.simple10.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
prd.simple10 <- predict(rf.simple10.fit,newdata = Train.Student.Data)
prd.simple10.table <- table(prd.simple10,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=5,repeats=5,search="random")
rf.random10.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
prd.random10 <- predict(rf.random10.fit,newdata = Train.Student.Data)
prd.random10.table <- table(prd.random10,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=5,repeats=5,search="grid")
rf.grid10.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
prd.grid10 <- predict(rf.grid10.fit,newdata = Train.Student.Data)
prd.grid10.table <- table(prd.grid10,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=20,repeats=10)
rf.simple20.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
prd.simple20 <- predict(rf.simple20.fit,newdata =Train.Student.Data)
prd.simple20.table <- table(prd.simple20,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=20,repeats=10,search="random")
rf.random20.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
prd.random20 <- predict(rf.random20.fit,newdata =Train.Student.Data)
prd.random20.table <- table(prd.random20,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=20,repeats=10,search="grid")
rf.grid20.fit <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
prd.grid20 <- predict(rf.grid20.fit,newdata =Train.Student.Data)
prd.grid20.table <- table(prd.grid20,Train.Student.Data$internet)

prd.simple10.table
prd.random10.table
prd.grid10.table
prd.simple20.table
prd.random20.table
prd.grid20.table


#Selecting Random Forest with Random search and repeated CV with 20 trees
prd.random20.table


#implementation of Adaboost

boost100.fit <- boosting(internet~.,data=Train.Student.Data,boos=TRUE,mfinal=100)
importanceplot(boost100.fit,las=2)
prdict.boost100 <-predict(boost100.fit,newdata=Train.Student.Data)
prdict.boost100$confusion


#------------------------------------------------------------------------------------------------------------------------
#SELECTION
#------------------------------------------------------------------------------------------------------------------------

#Backward Logistic
sel.predBackward <- predict(object = backwardSelectionObject,newdata = Select.Student.Data[,-22],type = "response")
sel.fittedBackward <- ifelse(sel.predBackward > STHBackward$threshold,1,0)
sel.accuracyBackward <- table(sel.fittedBackward, Select.Student.Data[,"internet"])
sel.accuracyBackward

#LDA
sel.lda.pr <- predict(lda.fit, Select.Student.Data[,-22])
sel.fittedlda  <- ifelse(sel.lda.pr$posterior[,1] > STHLDA$threshold,"no","yes")
sel.accuracylda <- table(sel.fittedlda, Select.Student.Data[,"internet"])
sel.accuracylda


#Naive Bayes
sel.nb.pr <- predict(nb.fit, Select.Student.Data[,-22], type="raw")
sel.fittednb  <- ifelse(sel.nb.pr[,1] > STHNB$threshold,"no","yes")
sel.accuracynb <- table(sel.fittednb, Select.Student.Data[,"internet"])
sel.accuracynb


#Biased Tree
sel.prd.biased.tree <- predict(biased.tree,newdata =  Select.Student.Data,type="class")
sel.table.biased.tree <- table(sel.prd.biased.tree,Select.Student.Data$internet)
sel.table.biased.tree

#RF
sel.prd.random20 <- predict(rf.random20.fit,newdata =Select.Student.Data)
sel.prd.random20.table <- table(sel.prd.random20,Select.Student.Data$internet)
sel.prd.random20.table


#RF
sel.prd.boost100 <- predict(boost100.fit,newdata =Select.Student.Data)
sel.prd.boost100$confusion


sel.accuracyBackward
sel.accuracylda
sel.accuracynb
sel.table.biased.tree
sel.prd.random20.table
sel.prd.boost100$confusion




