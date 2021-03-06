#KNN Classifier
library(caTools)
library(class)
library(gmodels)
library(caret)
library(randomForest)
library(car)

#Retreving the dataset from the location
setwd("C:/Users/DINESH GUDIVADA/Downloads/ML")
data<- read.csv("diabetic_data__KNN_Logistic.csv",stringsAsFactors = FALSE,header = TRUE, sep = ",")
str(data)

nrow(data)

#Remove NA
data1<-na.omit(data)

#row count
n = nrow(data1)

#Random sample taken from data
indexes = sample(n,n*(11/100))

#Sub set with approx 12k rows
data_new = data1[indexes,]
data_new

#Deleting the null values from the new dataset
is.na(data_new)

#w <- as.data.frame (scale(data_new[12]))
#w

#Dividing the data for training and testing
data_train <- data_new[1:7000, ]
data_test <- data_new[7001:11111,]

data_train_labels <- data_new[1:7000, 12]
data_test_labels <- data_new[7001:11111, 12]

str(data_train_labels)
str(data_new)
data_new

#Changing the datatype to other datatype
data_new$admission_type_id <- as.factor(data_new$admission_type_id)
data_new$discharge_disposition_id  <- as.factor(data_new$discharge_disposition_id)
data_new$admission_source_id  <- as.factor(data_new$admission_source_id)
data_new$time_in_hospital  <- as.factor(data_new$time_in_hospital)
data_new$num_lab_procedures  <- as.factor(data_new$num_lab_procedures)
data_new$num_procedures  <- as.factor(data_new$num_procedures)
data_new$num_medications  <- as.factor(data_new$num_medications)
data_new$number_outpatient  <- as.factor(data_new$number_outpatient)
data_new$number_emergency  <- as.factor(data_new$number_emergency)
data_new$number_inpatient  <- as.factor(data_new$number_inpatient)
data_new$number_diagnoses  <- as.factor(data_new$number_diagnoses)
data_new$diabetesMed  <- as.factor(data_new$diabetesMed)


set.seed(12345)

#Performing the Knn algorithm
p <- knn(train = data_train,test = data_test, cl = data_train_labels,10) 

table(data_test_pred) 
table(data_test_labels) 

#Predicting the test data
pred<-predict(p,newdata= data_train)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- data_train$diabetesMed

table(y_pred)
table(y_act)
#----------------------------------------------------------------------------
#Evaluation Methods
#CrossTable(x = y_pred , y = y_act, prop.chisq=FALSE)

confusionMatrix(factor(data_test$diabetesMed), data = p) 

rsquare = (cor(pred,y_act))^2
rsquare

#--------------------------------------------------------------------------

#data_test_pred <- knn(train = data_train, test = data_test, cl = data_train_labels, 10)






