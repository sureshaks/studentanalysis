###Forward subset selection
###Lasso subset selection
###Evaluate on test dataset-forward
###Evaluate on test dataset-Lasso
###Try best subset selection

load(file = "C:/Users/saich/Documents/SMLProject/DataObject.RData")
################################################################################################################################
#Forward subset selection:
#Create a null model, to go forward

forwardsselectionobject <- step(object =  glm(formula = internet ~ 1, data = Train.Student.Data,family = binomial),scope =  ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu 
                                + Mjob + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup + famsup
                                + paid + activities + nursery + higher + romantic + famrel+ freetime + goout + Dalc + 
                                  Walc + health + absences + G1 + G2 + G3 ,direction = "forward")

forwardsselectionobject$formula
###############################################################################################################
################################################################################################################################
#Best subsets selection:
library(leaps)
regsubsets.out <-
  regsubsets(internet ~ .,
             data = Train.Student.Data,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
summary.out <- summary(regsubsets.out)
bestSubsetSelectedVariables <- summary.out$which[which.max(summary.out$adjr2),]
################################################################################################################################
#LASSO subset selection:
Train.Student.Data$internet <- ifelse(Train.Student.Data$internet == "yes",1,0)
attach(Train.Student.Data)
modelMatrix <- model.matrix(internet ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
                              reason + guardian + traveltime + studytime + failures + schoolsup 
                            + famsup + paid + activities + nursery + higher + romantic + famrel + freetime +
                              goout + Dalc + Walc + health + absences + G1 + G2 + G3)
glmmod <- glmnet(modelMatrix, y=as.factor(internet), alpha=1, family="binomial")
plot(glmmod, xvar="lambda")
coef(glmmod)[, 10]
cv.glmmod <- cv.glmnet(modelMatrix, y=internet, alpha=1)
plot(cv.glmmod)
(best.lambda <- cv.glmmod$lambda.min)
pred.coef <- predict(glmmod, s = best.lambda, mode="norm.fraction", type="coefficients")
#We subset based on the coefficients obtained above
################################################################################################################
#Model evaluation based on Forward subset selection:
forwardSubsettedModel <- glm(formula =  forwardsselectionobject$formula,
                             family = binomial,data = Train.Student.Data)

predForward <- predict(object = forwardSubsettedModel,newdata = Test.Student.Data[,-22],type = "response")
fittedForward <- ifelse(predForward > 0.5,1,0)
accuracyForward <- table(fittedForward, Test.Student.Data[,"internet"])
AccuracyOfForward <- sum(diag(accuracyForward))/sum(accuracyForward)
#################################################################################################################
#Model evaluation based on LASSO subset selection:
detach(Train.Student.Data)
Test.Student.Data$internet <- ifelse(Test.Student.Data$internet == "yes",1,0)
modelMatrix <- NULL
attach(Test.Student.Data)
modelMatrix <- model.matrix(internet ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
                              reason + guardian + traveltime + studytime + failures + schoolsup 
                            + famsup + paid + activities + nursery + higher + romantic + famrel + freetime +
                              goout + Dalc + Walc + health + absences + G1 + G2 + G3)

LassoSubsettedModel <- glm(formula = internet ~ schoolMS+ Medu+ Fedu + Mjobservices + Fjobservices 
                           + guardianother + traveltime+ famrel + freetime+ G3,
                           family = binomial,data = as.data.frame(modelMatrix))
pred <- predict(object = LassoSubsettedModel,newdata = as.data.frame(modelMatrix),type = "response")
fittedLassoResults <- ifelse(pred > 0.5,"yes","no")
accuracyLasso <- table(fittedLassoResults, Test.Student.Data[,"internet"])
AccuracyOfLasso <- sum(diag(accuracyLasso))/sum(accuracyLasso)
###################################################################################################################
###Model Evaluation using best subset selection:
Test.Student.Data$internet <- ifelse(Test.Student.Data$internet == "yes",1,0)
modelMatrix <- NULL
attach(Test.Student.Data)
modelMatrix <- model.matrix(internet ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
                              reason + guardian + traveltime + studytime + failures + schoolsup 
                            + famsup + paid + activities + nursery + higher + romantic + famrel + freetime +
                              goout + Dalc + Walc + health + absences + G1 + G2 + G3)

BestSubsettedModel <- glm(formula = internet ~ schoolMS+ age+famsizeLE3+PstatusT+Medu+ Mjobhealth + Mjobother+Mjobservices +Mjobteacher
                          + Fjobother+Fjobservices+guardianmother+ guardianother + traveltime+studytime+failures+famsupyes+romanticyes
                          + famrel + freetime+ Walc+health+G3,
                           family = binomial,data = as.data.frame(modelMatrix))
predBest <- predict(object = BestSubsettedModel,newdata = as.data.frame(modelMatrix),type = "response")
fittedBestResults <- ifelse(predBest > 0.5,"yes","no")
accuracyBestSS <- table(fittedBestResults, Test.Student.Data[,"internet"])
AccuracyOfBestSS <- sum(diag(accuracyBestSS))/sum(accuracyBestSS)
#######################################################################################################################################








