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

# Load Data ---
data = read_csv("data.csv")
str(data)

# Randomize Data (If needed. Set max observations) ---
set.seed(12345)
data_rand = data[order(runif(1000)),]

# Split Data + Remove Incomplete Cases ---
training = data_rand[1:900,]
train = training[complete.cases(training),]
testing = data_rand[901:1000,]
test = testing[complete.cases(testing),]

# Get Factorials (replace X)
train$X = as.factor(train$X)
test$X = as.factor(test$X)

# Decision Tree Modeling  ---
## C5.0 (replace x)
data_model = C5.0(x = train[-1], y = train$X)
summary(data_model)

## rpart2 (replace x)
fit.rpart2 = train(X~., data = train, method="rpart2", metric=metric, trControl=control)
summary(fit.rpart2$finalModel)

# Evaluating the Model (replace x) ---
data_pred = predict(data_model, test)
CrossTable(test$X, data_pred, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('Actual X', 'Predicted X'))
(p = table(data_pred, test$X))
(Accuracy = sum(diag(p))/sum(p)*100)

## Check Error Rate (replace X)
error.rate = round(mean(data.pred != test$X,2))
error.rate

## Visualization
fancyRpartPlot(fit.rpart2$finalModel)
