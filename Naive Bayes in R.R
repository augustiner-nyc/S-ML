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

# Naive Bayes Modeling (replace X)  ---
naive_model = naive_bayes(X ~., data=train)
naive_model

# Naive Model Eval (replace X) ---
(conf_nat = table(predict(naive_model, test), test$X))
(accuracy = sum(diag(conf_nat))/sum(conf_nat)*100)


filteredTestPred = predict(naive_model, newdata=test)
p = table(filteredTestPred, test$X)
(Accuracy = sum(diag(p))/sum(p)*100)


## Check Error Rate (replace X)
error.rate = round(mean(data.pred != test$X,2))
error.rate
