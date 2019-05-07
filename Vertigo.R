library(tidyverse)
library(caret)
library(mlbench)
library(glmnet)
library(glmnetUtils)
library(ROCR)
library(fastDummies)
library(dplyr)
library(sqldf)
library(FNN)
library(class)
library(knncat)
library(e1071) 
library(naivebayes)
library(scales)
library(psych)

#Setting up working directory and reading dataset
setwd("/Users/coolk/Documents/MS/2019/Data Mining/Data_mining Final/")
Vertigo_data <- read.csv("Vertigo.csv", header = T, colClasses = "factor")
str(Vertigo_data)

view(Vertigo_data)
#pre-processing of the data.
Vertigo_data$symptoms_q1a <- gsub( "\\s.*", "", Vertigo_data$symptoms_q1a )
Vertigo_data <- Vertigo_data[,-1]

#Checking for NA values
length(which(is.na(Vertigo_data)))
p <- data.frame(names(Vertigo_data))

summary(Vertigo_data$BPPV)
#Features Plots 

plot(Vertigo_data$BPPV, col("blue"))

ggplot(data=Vertigo_data, aes(Vertigo_data$Age, fill = BPPV)) + 
  geom_bar(aes(stat = "count")) +
  labs(title = "Age Distribution Vs BPPV", x = "Age Distribution", y = "Count")

ggplot(data = Vertigo_data) +
  geom_scatterplot(mapping = aes(x = BPPV, y = Age))

ggplot(data = Vertigo_data) + geom_histogram(mapping = aes(x = BPPV, y = Age))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$Age)) + geom_jitter(aes(color = BPPV))

ggplot(data=Vertigo_data, aes(BPPV,Age)) + geom_jitter(aes(color = Age))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$symptoms_q1a)) + 
  geom_jitter(aes(color = symptoms_q1a)) + 
  labs(title =" Symptoms Vs BPPV", x = "BPPV", y = "Initial Symptoms") 

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$Age)) + geom_jitter(aes(color = symptoms_q2))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$Age)) + geom_jitter(aes(color = symptoms_q5))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$Age)) + geom_jitter(aes(color = symptoms_q6))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$Age)) + geom_jitter(aes(color = symptoms_q7))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$duration_q1)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$duration_q3a)) + 
  geom_jitter(aes(color = BPPV)) + 
  labs(title = "Symptoms Duration Vs BPPV", x = "BPPV", y = "Duration Seconds to Minute")
 

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$duration_q3d)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$duration_q3e)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$duration_q3g)) + geom_jitter(aes(color = BPPV))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q4)) + 
  geom_jitter(aes(color = BPPV)) +
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Lying OR Rolling on bed")
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q7)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q8)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q12)) + 
  geom_jitter(aes(color = BPPV)) +
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Sitting OR Standing")

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q13)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q15)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q17)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q18)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno1_q19)) + 
  geom_jitter(aes(color = BPPV)) +
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Reaching OR Bending")

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q4)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q11)) + 
  geom_jitter(aes(color = BPPV)) +
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Headache Throbs OR Pulses")
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q13)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q14)) + 
  geom_jitter(aes(color = BPPV)) + 
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Sensitivity to light")
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q15)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$yesno2_q17)) + geom_jitter(aes(color = BPPV))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$ear_q5)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$ear_q7)) + geom_jitter(aes(color = BPPV))
ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV,Vertigo_data$ear_q8)) + geom_jitter(aes(color = BPPV))

ggplot(data=Vertigo_data, aes(Vertigo_data$BPPV, fill = yesno1_q4)) + 
  geom_bar(aes(stat = "count", position_dodge())) + facet_grid(yesno1_q8 ~ .)


summary(Vertigo_data$BPPV)

ggplot(data=Vertigo_data, aes(Age)) + 
  geom_histogram(aes(color = Age)) + 
  labs(title = "Symptoms Vs BPPV", x = "BPPV", y = "Sensitivity to light")

hist(Vertigo_data$Age, col="Blue")

#Converting variables to dummy variables
Vertigo_data$symptoms_q1a <- ifelse(Vertigo_data$symptoms_q1a == "Not", "None",
                                   ifelse(Vertigo_data$symptoms_q1a == "None", "None",Vertigo_data$symptoms_q1a))

Vertigo_data$symptoms_q2 <- ifelse(Vertigo_data$symptoms_q2 == "More than once", "1",
                                     ifelse(Vertigo_data$symptoms_q2 == "Once", "0",Vertigo_data$symptoms_q2))

Vertigo_data$symptoms_q5 <- ifelse(Vertigo_data$symptoms_q5 == "Yes", "1",
                                   ifelse(Vertigo_data$symptoms_q5 == "No", "0",Vertigo_data$symptoms_q5))

Vertigo_data$symptoms_q6 <- ifelse(Vertigo_data$symptoms_q6 == "Yes", "1",
                                   ifelse(Vertigo_data$symptoms_q6 == "No", "0",Vertigo_data$symptoms_q6))

Vertigo_data$symptoms_q7 <- ifelse(Vertigo_data$symptoms_q7 == "Yes", "1",
                                   ifelse(Vertigo_data$symptoms_q7 == "No", "0",Vertigo_data$symptoms_q7))

Vertigo_data$duration_q1 <- ifelse(Vertigo_data$duration_q1 == "Yes", "1",
                                   ifelse(Vertigo_data$duration_q1 == "No", "0",Vertigo_data$duration_q1))

Vertigo_data$duration_q3a <- ifelse(Vertigo_data$duration_q3a == "Yes", "1",
                                   ifelse(Vertigo_data$duration_q3a == "No", "0",Vertigo_data$duration_q3a))

Vertigo_data$duration_q3d <- ifelse(Vertigo_data$duration_q3d == "Yes", "1",
                                   ifelse(Vertigo_data$duration_q3d == "No", "0",Vertigo_data$duration_q3d))

Vertigo_data$duration_q3e <- ifelse(Vertigo_data$duration_q3e == "Yes", "1",
                                   ifelse(Vertigo_data$duration_q3e == "No", "0",Vertigo_data$duration_q3e))

Vertigo_data$duration_q3g <- ifelse(Vertigo_data$duration_q3g == "Yes", "1",
                                   ifelse(Vertigo_data$duration_q3g == "No", "0",Vertigo_data$duration_q3g))

Vertigo_data$yesno1_q4 <- ifelse(Vertigo_data$yesno1_q4 == "Yes", "1",
                                   ifelse(Vertigo_data$yesno1_q4 == "No", "0",Vertigo_data$yesno1_q4))

Vertigo_data$yesno1_q7 <- ifelse(Vertigo_data$yesno1_q7 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno1_q7 == "No", "0",Vertigo_data$yesno1_q7))

Vertigo_data$yesno1_q8 <- ifelse(Vertigo_data$yesno1_q8 == "Yes", "1",
                                   ifelse(Vertigo_data$yesno1_q8 == "No", "0",Vertigo_data$yesno1_q8))

Vertigo_data$yesno1_q12 <- ifelse(Vertigo_data$yesno1_q12 == "Yes", "1",
                                   ifelse(Vertigo_data$yesno1_q12 == "No", "0",Vertigo_data$yesno1_q12))

Vertigo_data$yesno1_q13 <- ifelse(Vertigo_data$yesno1_q13 == "Yes", "1",
                                   ifelse(Vertigo_data$yesno1_q13 == "No", "0",Vertigo_data$yesno1_q13))

Vertigo_data$yesno1_q15 <- ifelse(Vertigo_data$yesno1_q15 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno1_q15 == "No", "0",Vertigo_data$yesno1_q15))

Vertigo_data$yesno1_q17 <- ifelse(Vertigo_data$yesno1_q17 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno1_q17 == "No", "0",Vertigo_data$yesno1_q17))

Vertigo_data$yesno1_q18 <- ifelse(Vertigo_data$yesno1_q18 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno1_q18 == "No", "0",Vertigo_data$yesno1_q18))

Vertigo_data$yesno1_q19 <- ifelse(Vertigo_data$yesno1_q19 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno1_q19 == "No", "0",Vertigo_data$yesno1_q19))

Vertigo_data$yesno2_q4 <- ifelse(Vertigo_data$yesno2_q4 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno2_q4 == "No", "0",Vertigo_data$yesno2_q4))

Vertigo_data$yesno2_q11 <- ifelse(Vertigo_data$yesno2_q11 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno2_q11 == "No", "0",Vertigo_data$yesno2_q11))

Vertigo_data$yesno2_q13 <- ifelse(Vertigo_data$yesno2_q13 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno2_q13 == "No", "0",Vertigo_data$yesno2_q13))

Vertigo_data$yesno2_q14 <- ifelse(Vertigo_data$yesno2_q14 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno2_q14 == "No", "0",Vertigo_data$yesno2_q14))

Vertigo_data$yesno2_q15 <- ifelse(Vertigo_data$yesno2_q15 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno2_q15 == "No", "0",Vertigo_data$yesno2_q15))

Vertigo_data$yesno2_q17 <- ifelse(Vertigo_data$yesno2_q17 == "Yes", "1",
                                 ifelse(Vertigo_data$yesno2_q17 == "No", "0",Vertigo_data$yesno2_q17))

Vertigo_data$ear_q5 <- ifelse(Vertigo_data$ear_q5 == "Yes. In one ear.", "1",
                          ifelse(Vertigo_data$ear_q5 == "Yes. In both ears.", "1",
                              ifelse(Vertigo_data$ear_q5 == "Yes", "1",
                                 ifelse(Vertigo_data$ear_q5 == "No", "0",Vertigo_data$ear_q5))))

Vertigo_data$ear_q7 <- ifelse(Vertigo_data$ear_q7 == "Yes. In one ear.", "1",
                          ifelse(Vertigo_data$ear_q7 == "Yes. In both ears.", "1",
                                ifelse(Vertigo_data$ear_q7 == "Yes", "1",      
                                     ifelse(Vertigo_data$ear_q7 == "No", "0",Vertigo_data$ear_q7))))

Vertigo_data$ear_q8 <- ifelse(Vertigo_data$ear_q8 == "Yes. In one ear.", "1",
                              ifelse(Vertigo_data$ear_q8 == "Yes. In both ears.", "1",
                                  ifelse(Vertigo_data$ear_q8 == "Yes", "1", 
                                     ifelse(Vertigo_data$ear_q8 == "No", "0",Vertigo_data$ear_q8))))

Vertigo_data$yesno3_q12 <- ifelse(Vertigo_data$yesno3_q12 == "Yes", "1",
                                  ifelse(Vertigo_data$yesno3_q12 == "No", "0",Vertigo_data$yesno3_q12))

Vertigo_data$Gender <- ifelse(Vertigo_data$Gender == "FEMALE", "1",
                                  ifelse(Vertigo_data$Gender == "MALE", "0",Vertigo_data$Gender))

Vertigo_data$BPPV <- ifelse(Vertigo_data$BPPV == "Yes", "1",
                                  ifelse(Vertigo_data$BPPV == "No", "0",Vertigo_data$BPPV))

#Partitnioning the data
set.seed(225)
trainIndex <- createDataPartition(Vertigo_data$BPPV, p = 0.8, list = FALSE, times = 1)

VertigoTrain <- Vertigo_data[trainIndex, ]

VertigoTest <- Vertigo_data[-trainIndex, ]

#modelling Naive Bayes Model with whole dataset.
Naive_model <- naive_bayes(BPPV ~ ., data=VertigoTrain)

#Predcitions

p1 <- predict(Naive_model, VertigoTrain)
(tab1 <- table(p1, VertigoTrain$BPPV))
#misclassification calculation
1 - sum(diag(tab1))/sum(tab1)
#Accuracy
confusionMatrix(tab1)

p2 <- predict(Naive_model, VertigoTest)
(tab2 <- table(VertigoTest$BPPV,p2))
#accuracy
confusionMatrix(tab2)

#misclassification calculation
1 - sum(diag(tab1))/sum(tab1) 


#modelling Naive Bayes Model with selected features.

Naive_model1 <- naive_bayes(BPPV ~ symptoms_q1a+duration_q3a+yesno1_q4+yesno1_q12+yesno1_q19+yesno2_q11+yesno2_q14+Age, data=VertigoTrain)

#Predcitions
plot(Naive_model1)

print(cbind(p1, VertigoTrain))

p1 <- predict(Naive_model1, VertigoTrain)
(tab1 <- table(p1, VertigoTrain$BPPV))
#Accuracy 
confusionMatrix(tab1)
#misclassification calculation
1 - sum(diag(tab1))/sum(tab1)

p2 <- predict(Naive_model1, VertigoTest)
(tab2 <- table(VertigoTest$BPPV,p2))
#accuracy
confusionMatrix(tab2)
percent(sum(diag(tab1))/sum(tab1))
#misclassification calculation
1 - sum(diag(tab1))/sum(tab1) 


#Naive Bayes with k fold verification
myvars <- c("symptoms_q1a", "duration_q3a", "yesno1_q4", "yesno1_q12", "yesno1_q19", "yesno2_q11", "yesno2_q14", "BPPV")

  Vertigo_Sel <- Vertigo_data[myvars]
  
  pairs.panels(Vertigo_Sel)
  
  train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 9)
  
  model <- train(BPPV ~ ., data=Vertigo_Sel, trControl=train_control, method = "nb")
  
print(model)
  
  
  
  
  