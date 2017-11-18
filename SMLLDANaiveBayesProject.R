# Required libraries
library(MASS)
library(ROCR)
library(e1071)

################################################################
# LDA
################################################################
# train the model
lda.fit <- lda(as.factor(internet) ~ as.factor(school) + 
      as.factor(sex) + 
      as.ordered(age) + 
      as.factor(address) + 
      as.factor(famsize) + 
      as.factor(Pstatus) + 
      as.ordered(Medu) + 
      as.ordered(Fedu) + 
      as.factor(Mjob) + 
      as.factor(Fjob) + 
      as.factor(reason) + 
      as.factor(guardian) + 
      as.ordered(traveltime) + 
      as.ordered(studytime) + 
      as.ordered(failures) + 
      as.factor(schoolsup) + 
      as.factor(famsup) + 
      as.factor(paid) + 
      as.factor(activities) + 
      as.factor(nursery) + 
      as.factor(higher) + 
      as.factor(romantic) + 
      as.ordered(famrel) + 
      as.ordered(freetime) + 
      as.ordered(goout) + 
      as.ordered(Dalc) + 
      as.ordered(Walc) + 
      as.ordered(health) + 
      absences + 
      G1 + 
      G2 + 
      G3, data = as.data.frame(Train.Student.Data))

# Training set Plot LDA ROC curve and LDA accuracy
lda.pr <- predict(lda.fit, Train.Student.Data[,-22])
lda.pred = prediction(lda.pr$posterior[,2], Train.Student.Data[,22])
lda.perf = performance(lda.pred, "tpr", "fpr")
plot(lda.perf, colorize=T, main="LDA")
lda.acc <- sum(lda.pr$class == Train.Student.Data[,22])/nrow(Train.Student.Data)
lda.area <- unlist(attributes(performance(lda.pred, "auc"))$y.values)

# Test set Plot LDA ROC curve and LDA accuracy
lda.pr.test <- predict(lda.fit, Test.Student.Data[,-22])
lda.pred.test = prediction(lda.pr.test$posterior[,2], Test.Student.Data[,22])
lda.perf.test = performance(lda.pred.test, "tpr", "fpr")
plot(lda.perf.test, colorize=T, main="LDA")
lda.acc.test <- sum(lda.pr.test$class == Test.Student.Data[,22])/nrow(Test.Student.Data)
lda.area.test <- unlist(attributes(performance(lda.pred.test, "auc"))$y.values)

################################################################
# Naive Bayes
################################################################
# train the model
nb.fit <- naiveBayes(as.factor(internet) ~ as.factor(school) + 
                       as.factor(sex) + 
                       as.ordered(age) + 
                       as.factor(address) + 
                       as.factor(famsize) + 
                       as.factor(Pstatus) + 
                       as.ordered(Medu) + 
                       as.ordered(Fedu) + 
                       as.factor(Mjob) + 
                       as.factor(Fjob) + 
                       as.factor(reason) + 
                       as.factor(guardian) + 
                       as.ordered(traveltime) + 
                       as.ordered(studytime) + 
                       as.ordered(failures) + 
                       as.factor(schoolsup) + 
                       as.factor(famsup) + 
                       as.factor(paid) + 
                       as.factor(activities) + 
                       as.factor(nursery) + 
                       as.factor(higher) + 
                       as.factor(romantic) + 
                       as.ordered(famrel) + 
                       as.ordered(freetime) + 
                       as.ordered(goout) + 
                       as.ordered(Dalc) + 
                       as.ordered(Walc) + 
                       as.ordered(health) + 
                       absences + 
                       G1 + 
                       G2 + 
                       G3, data = as.data.frame(Train.Student.Data))

# Training accuracy and ROC curve
nb.pr = list()
nb.pr$raw <- predict(nb.fit, Train.Student.Data[,-22], type="raw")
nb.pr$val <- predict(nb.fit, Train.Student.Data[,-22])
nb.pred <- prediction(nb.pr$raw[,2], Train.Student.Data[,22])
nb.perf <- performance(nb.pred, "tpr", "fpr")
plot(nb.perf, colorize=T, main="Naive Bayes")
nb.acc <- sum(nb.pr$val == Train.Student.Data[,22])/nrow(Train.Student.Data)
nb.area <- unlist(attributes(performance(nb.pred, "auc"))$y.values)

# Testing accuracy and ROC curve
nb.pr.test = list()
nb.pr.test$raw <- predict(nb.fit, Test.Student.Data[,-22], type="raw")
nb.pr.test$val <- predict(nb.fit, Test.Student.Data[,-22])
nb.pred.test <- prediction(nb.pr.test$raw[,2], Test.Student.Data[,22])
nb.perf.test <- performance(nb.pred.test, "tpr", "fpr")
plot(nb.perf.test, colorize=T, main="Naive Bayes")
nb.acc.test <- sum(nb.pr.test$val == Test.Student.Data[,22])/nrow(Test.Student.Data)
nb.area.test <- unlist(attributes(performance(nb.pred.test, "auc"))$y.values)