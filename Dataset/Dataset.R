setwd("C:/Users/nouls/Desktop/Supervised ML/Project/Internet at home/")
Student.Data  <- read.csv(file = "student-por.csv")
str(Student.Data)

attach(Student.Data)
   
  Student.Data$Medu <- as.ordered(Medu) 
  Student.Data$Fedu <-   as.ordered(Fedu)  
  Student.Data$traveltime <- as.ordered(traveltime)  
  Student.Data$studytime <- as.ordered(studytime)  
  Student.Data$failures <- as.ordered(failures)  

  
  str(Student.Data)
  
set.seed(400)
Index <- sample(x = 1:nrow(Student.Data),size = 0.6*nrow(Student.Data))
Train.Student.Data <- Student.Data[Index,]
Temp.Student.Data <- Student.Data[-Index,]
Index1 <- sample(x = 1:nrow(Temp.Student.Data),size = 0.5*nrow(Temp.Student.Data))
Select.Student.Data <- Temp.Student.Data[Index1,]
Test.Student.Data <- Temp.Student.Data[-Index1,]

rm(Index,Index1,Temp.Student.Data)

sum(Test.Student.Data$internet=="yes")/nrow(Test.Student.Data)
sum(Train.Student.Data$internet=="yes")/nrow(Train.Student.Data)
sum(Select.Student.Data$internet=="yes")/nrow(Select.Student.Data)

str(Train.Student.Data)

Train.Student.Data.Matrix <- model.matrix(~ .,Train.Student.Data)[,-1]
Select.Student.Data.Matrix <- model.matrix(~ .,Select.Student.Data)[,-1]
Test.Student.Data.Matrix <- model.matrix(~ .,Test.Student.Data)[,-1]

