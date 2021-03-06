#Logistic_Regression
library(caTools)
library(class)
library(caret)
library(psych)

#Retreving the dataset from the location
setwd("C:/Users/DINESH GUDIVADA/Downloads/Project")
data<- read.csv("Superstoredata_KNN_LOGISTIC_Multiple.csv",stringsAsFactors = FALSE,header = TRUE, sep = ",")
data

str(data)

nrow(data)

#Identify NA values
is.na(data)

#Remove NA
data1<-na.omit(data)

#row count
n = nrow(data1)

#Random sample taken from data
indexes = sample(n,n*(11/100))

#Sub set with approx 12k rows
data_new = data1[indexes,]
str(data_new)
data_new

#Checking for Null values in the cleaned dataset
is.na(data_new)

#Dividing the data for training and testing
data_train <- data_new[1:9000, ]
data_test <- data_new[9001:11128,]

#Viewing the data
str(data_train_labels)
str(data_new)
view(data_new)
data_new

#Converting the "yes" or "No" values to "1" or "0"
data_new$diabetesMed  <- factor(data_new$Profit, labels=c("yes","no"),levels = c(1,0))

#Performing the Logistic Regression Model
logit_mod<-glm(diabetesMed~.- number_emergency -admission_type_id -admission_source_id,family="binomial",data_train)
summary(logit_mod)
pred<-predict(logit_mod,newdata= data_train)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- data_train$diabetesMed
mean(y_pred == y_act)


table(y_pred)
table(y_act)

#--------------------------------------------------------------------------
#Evaluation Methods
CrossTable(x = y_pred, y = y_act, prop.chisq=FALSE)

confusionMatrix(factor(data_test$diabetesMed), data = logit_mod) 

rsquare = (cor(pred,y_act))^2
rsquare
#--------------------------------------------------------------------------
pairs.panels(data_new)

#rf <- knn(diabetesMed ~ . -encounter_id	-patient_nbr	-race	-gender
#-readmitted -age -diag_1 -diag_2 -diag_3 -number_diagnoses	-max_glu_serum	-A1Cresult	-metformin	-repaglinide	-nateglinide	-chlorpropamide	-glimepiride	-acetohexamide	-glipizide	-citoglipton	-insulin , data = data_new)
#rf