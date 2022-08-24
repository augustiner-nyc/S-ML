# Intitialize Library ---
library(C50)
library(quantmod)
library(readr)
library(mice)
library(magrittr)
library(caret)
library(TTR)
library(corrplot)
library(logr)
library(C50)
library(gmodels)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(naivebayes)

# Load Data ---
data = read_csv("data.csv")
str(data)

# Split Data + Remove Incomplete Cases ---
index = createDataPartition(iris[,1], p =0.80, list = FALSE)
training = data[index,]
testing = data[-index,]
train = training[complete.cases(training),]
test = testing[complete.cases(testing),]

# Get Factorials (replace X)
train$X = as.factor(train$X)
test$X = as.factor(test$X)

# Support Vector Machines (replace X) ---
X_classifier = ksvm(X ~ ., data = train)
summary(X_classifier)

# SVM Model Eval (replace X) ---
X_pred = predict(X_classifier, test)
(p = table(X_pred, test$X))
(Accuracy = sum(diag(p))/sum(p)*100)


## Check Error Rate (replace X)
error.rate = round(mean(X.pred != test$X,2))
error.rate
