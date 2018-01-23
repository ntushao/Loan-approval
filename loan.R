library(tidyverse)
library(caret)
library(InformationValue)
library(doSNOW)
library(mice)

#Read and combine the data
setwd("~/Downloads")
loan.test <- read.csv("test_Y3wMUE5_7gLdaTN.csv",stringsAsFactors=FALSE)
loan.train <- read.csv("train_u6lujuX_CVtuZ9i.csv",stringsAsFactors=FALSE)
View(loan.test)
View(loan.train)
full <- bind_rows(loan.train,loan.test)
str(full)
View(full)

#Recode some variable values
full$Dependents[full$Dependents=="3+"] <- "3"
full$Education[full$Education=="Graduate"] <- "1"
full$Education[full$Education=="Not Graduate"] <- "0"


#Feature selection and statistics
prop.table(table(loan.train$Gender,loan.train$Loan_Status),1)
chisq.test(table(loan.train$Gender,loan.train$Loan_Status))

prop.table(table(loan.train$Married,loan.train$Loan_Status),1)
chisq.test(table(loan.train$Married,loan.train$Loan_Status))

prop.table(table(loan.train$Education,loan.train$Loan_Status),1)
chisq.test(table(loan.train$Education,loan.train$Loan_Status))


prop.table(table(loan.train$Self_Employed,loan.train$Loan_Status),1)
chisq.test(table(loan.train$Self_Employed,loan.train$Loan_Status))

loan.train<- mutate(loan.train,TotalIncome=ApplicantIncome+CoapplicantIncome)
loan.train<- mutate(loan.train,ratio=LoanAmount/TotalIncome)

t.test(ratio~Loan_Status,data=loan.train)

ggplot(loan.train,aes(x=Loan_Status,y=ratio))+geom_boxplot()
chisq.test(table(factor(loan.train$Loan_Amount_Term),loan.train$Loan_Status))
loan.train$status[loan.train$Loan_Status=="N"] <- 0
loan.train$status[loan.train$Loan_Status=="Y"] <- 1

#Build a new variable 'ratio'
loan.train<- mutate(loan.train,ratio=LoanAmount/(ApplicantIncome+CoapplicantIncome))
a <- glm(status~ratio+Dependents+Education,data=loan.train,family = binomial)
summary(a)

t.test(LoanAmount~Loan_Status,data=loan.train1)
t.test(TotalIncome~Loan_Status,data=loan.train1)


chisq.test(table(loan.train$Property_Area,loan.train$Loan_Status))

prop.table(table(loan.train$Credit_History,loan.train$Loan_Status),1)






sapply(full,function(x) sum(is.na(x)))
sapply(full,function(x) sum(x==""))
full <- full[,-1]
full$Dependents[full$Dependents==""&full$Married=="No"] <- "0"
full$Dependents[full$Dependents==""&full$Married=="Yes"] <- "3"
full$Dependents[full$Dependents==""&full$CoapplicantIncome==0] <- "0"
full$Dependents[full$Dependents==""&full$CoapplicantIncome!=0] <- "3"
full$Gender[full$Gender=="Missing"] <- "Male"
full$Married[full$Married==""] <- "Missing"
full$Self_Employed[full$Self_Employed==""] <- "Missing"


table(full$Credit_History)

#Include ratio,Dependents,Education, Credit_History, Property_Area and Married

full$Gender <- factor(full$Gender)
full$Married <- factor(full$Married)
full$Dependents <- factor(full$Dependents)
full$Education <- factor(full$Education)
full$Self_Employed <- factor(full$Self_Employed)
full$Loan_Amount_Term <- factor(full$Loan_Amount_Term)
full$Property_Area <- factor(full$Property_Area)
full$Loan_Status <- factor(full$Loan_Status)




#Fill in missing values for credit history and assign the value to 1 if it is >0.5; otherwise assign it to 0.

dummy <- dummyVars(~.,data=full[,-12])
trainDummy <- predict(dummy,full[,-12])
View(trainDummy)
Pre <- preProcess(trainDummy,method="bagImpute")
imputed <- predict(Pre,trainDummy)
View(imputed)
imputed <- as.data.frame(imputed)
imputed$Credit_History[imputed$Credit_History>=0.5] <- 1
imputed$Credit_History[imputed$Credit_History<0.5] <- 0

full$LoanAmount <- imputed$LoanAmount
full$Credit_History <- imputed$Credit_History
full$Credit_History <- factor(full$Credit_History)

full <- mutate(full,ratio=LoanAmount/(ApplicantIncome+CoapplicantIncome))
full <- select(full,ratio,Dependents,Education, Credit_History, Property_Area,Loan_Status,Married)
full <- mutate(full,logratio=log(ratio))
full <- select(full,-ratio)
train <- full[1:614,]
test <- full[615:981,]

#Train the data using random forest and make prediction.
train.control <- trainControl(method="repeatedcv",number=10,repeats=5,search="grid")
tune <- expand.grid(mtry=1:8)
training <- train(Loan_Status~.,data=train,method="rf",tuneGrid=tune,trControl=train.control)

predicting <- predict(training,test)
solution <- data.frame(Loan_ID = loan.test$Loan_ID, Loan_Status = predicting)
write.csv(solution,file="luck130.csv",row.names = F)


