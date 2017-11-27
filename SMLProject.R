Student.Data1  <- read.csv(file = "C:\\Users\\saich\\Downloads\\student-por.csv")
any(is.na(Student.Data1))
summary(Student.Data1)
set.seed(400)
Index <- sample(x = 1:nrow(Student.Data1),size = 0.8*nrow(Student.Data1))
Train.Student.Data1 <- Student.Data1[Index,]
Test.Student.Data1 <- Student.Data1[-Index,]
#Checking 
Proportion.Internet.Train1 <- Train.Student.Data1 %>% summarise(mean(internet=="yes"))
Proportion.Internet.Test1<- Test.Student.Data1 %>% summarise(mean(internet=="yes"))

Train.Student.Data.Matrix <- model.matrix(~ .,Train.Student.Data)[,-1]
a <- cor(Train.Student.Data.Matrix)

#Mothers education and fathers education are correlated
#walc and dalc are correlated
#Mothers job as teacher and mothers education are correlated
#G1,G2 and G3 are interrelated
#As per correlation values there are no strong relations other than the grades

#Creating a boxplot
boxplot(scale(Train.Student.Data[,names(Train.Student.Data[,sapply(Train.Student.Data, is.numeric)])]),las=2)
#There seems to be an outlier for age which is not alarming
#There seems to be an outlier for absences but all other predictors are within range so this observation need not be excluded as of now

pairs(x = Train.Student.Data[,names(Train.Student.Data[,sapply(Train.Student.Data, is.numeric)])])
#pairs(x = Train.Bank.Marketing.Data[Train.Bank.Marketing.Data$job == "retired",c("age","job")])

#Train.Bank.Marketing.Data.Retired <- Train.Bank.Marketing.Data[Train.Bank.Marketing.Data$job =="retired",c("age","job")]
#ggplot(Train.Bank.Marketing.Data) + geom_point(mapping = aes(x = age,y=job),position = "jitter",alpha=0.5)


###################################################################################################################################
###Backward subset selection:
backwardSelectionObject <- step(glm(formula = internet ~ ., data = Train.Student.Data, family = binomial),
                                direction = "backward", trace=F)
backwardPredictions <- predict(object = backwardSelectionObject,newdata = Test.Student.Data[,-22],type = "response")







