library("caret")
library("e1071")


install.packages("randomForest")
library("randomForest")
internet.rf <- train(internet~., data=Train.Student.Data,type="rf")
internet.rf

plot(internet.rf)
str(internet.rf)
prd <- predict(internet.rf,newdata=Train.Student.Data)

table(prd,Train.Student.Data$internet)

505/519

tcontrol = trainControl(method="repeatedcv",number=5,repeats=5)
internet.rf1 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
internet.rf1
prd1 <- predict(internet.rf1,newdata =  Train.Student.Data)
table(prd1,Train.Student.Data$internet)

tcontrol = trainControl(method="repeatedcv",number=5,repeats=5,search="random")
internet.rf2 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
internet.rf2

tcontrol = trainControl(method="repeatedcv",number=5,repeats=5,search="grid")
internet.rf3 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=10, trControl=tcontrol)
internet.rf3


tcontrol = trainControl(method="repeatedcv",number=10,repeats=4)
internet.rf11 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
internet.rf11

tcontrol = trainControl(method="repeatedcv",number=20,repeats=10,search="random")
internet.rf22 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
internet.rf22

tcontrol = trainControl(method="repeatedcv",number=20,repeats=10,search="grid")
internet.rf33 <- train(internet~., data=Train.Student.Data,metric="Accuracy",type="rf",ntree=20, trControl=tcontrol)
internet.rf33


prd.tst <- predict(internet.rf,Test.Student.Data)
table(prd.tst,Test.Student.Data$internet)

prd.tst1 <- predict(internet.rf1,Test.Student.Data)
table(prd.tst1,Test.Student.Data$internet)

prd.tst2 <- predict(internet.rf2,Test.Student.Data)
table(prd.tst2,Test.Student.Data$internet)

prd.tst3 <- predict(internet.rf3,Test.Student.Data)
table(prd.tst3,Test.Student.Data$internet)

prd.tst11 <- predict(internet.rf11,Test.Student.Data)
table(prd.tst11,Test.Student.Data$internet)

prd.tst22 <- predict(internet.rf22,Test.Student.Data)
table(prd.tst22,Test.Student.Data$internet)

prd.tst33 <- predict(internet.rf33,Test.Student.Data)
table(prd.tst33,Test.Student.Data$internet)


install.packages("adabag")
library(adabag)

internet.boost1 <- boosting(internet~.,data=Train.Student.Data,boos=TRUE,mfinal=100)
predict.train.boost1 <- predict(internet.boost1,newdata=Train.Student.Data)
importanceplot(internet.boost1)
predict.train.boost1$confusion
predict.test.boost1 <- predict(internet.boost1,newdata=Test.Student.Data)
predict.test.boost1$confusion
error11 <- errorevol(internet.boost1,newdata=Train.Student.Data)
error12 <- errorevol(internet.boost1,newdata=Test.Student.Data)
plot.errorevol(error2,error1)


internet.boost2 <- boosting(internet~.,data=Train.Student.Data,boos=TRUE,mfinal=500)
predict.train.boost2 <- predict(internet.boost2,newdata=Train.Student.Data)
importanceplot(internet.boost2)
predict.train.boost2$confusion
predict.test.boost2 <- predict(internet.boost2,newdata=Test.Student.Data)
predict.test.boost2$confusion
error21 <- errorevol(internet.boost1,newdata=Train.Student.Data)
error22 <- errorevol(internet.boost1,newdata=Test.Student.Data)
plot.errorevol(error22,error21)




internet.bagging1 <- bagging(internet~.,data=Train.Student.Data,mfinal=50)
predict.train.bagging1 <- predict(internet.bagging1,newdata=Train.Student.Data)
importanceplot(internet.bagging1)
predict.train.bagging1$confusion
predict.test.bagging1 <- predict(internet.bagging1,newdata=Test.Student.Data)
predict.test.bagging1$confusion
error1 <- errorevol(internet.bagging1,newdata=Train.Student.Data)
error2 <- errorevol(internet.bagging1,newdata=Test.Student.Data)
plot.errorevol(error2,error1)

internet.nb1 <- naiveBayes(internet~.,Train.Student.Data,type="raw")
prd1 <- predict(internet.nb1,newdata =  Train.Student.Data)
table(prd1,Train.Student.Data$internet)
(324+66)/(324+66+76+53)

prd.tst1 <- predict(internet.nb1,Test.Student.Data)
table(prd.tst1,Test.Student.Data$internet)
(94)/(94+14+22)



internet.tree1 <- tree(internet~.,Train.Student.Data)
plot(internet.tree1)
text(internet.tree1)
prd1 <- predict(internet.tree1,newdata =  Train.Student.Data,type="class")
table(prd1,Train.Student.Data$internet)
prd.tst1 <- predict(internet.tree1,Test.Student.Data,type="class")
table(prd.tst1,Test.Student.Data$internet)


prd1 <- predict(internet.tree1,newdata =  Train.Student.Data)
table(ifelse(prd1[,1]>0.4,"no","yes"),Train.Student.Data$internet)
prd.tst1 <- predict(internet.tree1,Test.Student.Data)
table(ifelse(prd.tst1[,1]>0.4,"no","yes"),Test.Student.Data$internet)
(89)/(89+25+16)

Train.Student.Data.biased <- rbind(Train.Student.Data,Train.Student.Data[Train.Student.Data$internet=="no",],Train.Student.Data[Train.Student.Data$internet=="no",],Train.Student.Data[Train.Student.Data$internet=="no",],Train.Student.Data[Train.Student.Data$internet=="no",],Train.Student.Data[Train.Student.Data$internet=="no",])
internet.tree2 <- tree(internet~.,Train.Student.Data.biased)
plot(internet.tree2)
text(internet.tree2)
prd1 <- predict(internet.tree2,newdata =  Train.Student.Data.biased,type="class")
table(prd1,Train.Student.Data.biased$internet)
prd.tst1 <- predict(internet.tree2,Test.Student.Data,type="class")
table(prd.tst1,Test.Student.Data$internet)
table(ifelse(prd.tst1[,1] >0.1,0,1),Test.Student.Data$internet)


