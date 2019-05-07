library(caret)
library(e1071)
library(plyr)
library(arules)
library(arulesViz)

#Setting up working directory and reading dataset
setwd("/Users/coolk/Documents/MS/2019/Data Mining/Data_mining Final/")
Vertigo_data <- read.csv("Vertigo.csv", header = T, colClasses = "factor")

#pre-processing of the data.
Vertigo_data$symptoms_q1a <- gsub( "\\s.*", "", Vertigo_data$symptoms_q1a )
Vertigo_data <- Vertigo_data[,-1]

Vertigo_data <- subset(Vertigo_data, select = -c(patientid, symptoms_q2))


Vertigo_data$ear_q5 <- ifelse(Vertigo_data$ear_q5 == "Yes. In one ear.", "Yes",
                              ifelse(Vertigo_data$ear_q5 == "Yes. In both ears.", "Yes",
                                     ifelse(Vertigo_data$ear_q5 == "Yes", "Yes",
                                            ifelse(Vertigo_data$ear_q5 == "No", "No",Vertigo_data$ear_q5))))

Vertigo_data$ear_q7 <- ifelse(Vertigo_data$ear_q7 == "Yes. In one ear.", "Yes",
                              ifelse(Vertigo_data$ear_q7 == "Yes. In both ears.", "Yes",
                                     ifelse(Vertigo_data$ear_q7 == "Yes", "Yes",      
                                            ifelse(Vertigo_data$ear_q7 == "No", "No",Vertigo_data$ear_q7))))

Vertigo_data$ear_q8 <- ifelse(Vertigo_data$ear_q8 == "Yes. In one ear.", "Yes",
                              ifelse(Vertigo_data$ear_q8 == "Yes. In both ears.", "Yes",
                                     ifelse(Vertigo_data$ear_q8 == "Yes", "Yes", 
                                            ifelse(Vertigo_data$ear_q8 == "No", "No",Vertigo_data$ear_q8))))
str(Vertigo_data)


col <- c("symptoms_q1a","ear_q5","ear_q7","ear_q8")
Vertigo_data[col] <- lapply(Vertigo_data[col], factor)

length(which(is.na(Vertigo_data)))
#checking datatype for the variable.
str(Vertigo_data)


#applying apriori on titanic dataset by default variable.
rules1 <- apriori(Vertigo_data, parameter = list(minlen=2, supp=.5, conf=0.9))

summary(rules1)
plot(rules1)
inspect(rules1)

rules2 <- apriori(Vertigo_data, parameter = list(minlen=2, supp=.5, conf=0.9), 
                  appearance=list(rhs=c("ear_q8=No"), default="lhs"))
summary(rules2)

inspect(rules2)

plot(rules2)

summary(Vertigo_data)

rules3 <- apriori(titanic, parameter = list(supp=.7, conf=0.7))

summary(rules3)

inspect(rules2)

