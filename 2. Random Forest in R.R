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
library(AppliedPredictiveModeling)

# Load Data ---
data = read_csv("data.csv")
str(data)

# Randomize Data (If needed) ---
set.seed(12345)

# Visualize the data ---
## Scatterplot Matrix (replace X)
transparentTheme(trans = .4)
featurePlot(x = data[, 1:4], #Columns
            y = data$X, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

## Overlayed Density Plot (replace X)
transparentTheme(trans = .9)
featurePlot(x = data[, 1:4], #Columns
            y = data$X,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


## Box Plots (replace X)
featurePlot(x = data[, 1:4], 
            y = dataa$X, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

# Split Data + Remove Incomplete Cases ---
index = createDataPartition(iris[,1], p =0.80, list = FALSE)
training = data[index,]
testing = data[-index,]
train = training[complete.cases(training),]
test = testing[complete.cases(testing),]

# Get Factorials (replace X)
train$X = as.factor(train$X)
test$X = as.factor(test$X)

# Random Forest Modeling (replace x) ---
fit.rf = train(X~., data = training, method="rf", metric=metric, trControl=control)
fit.rf
summary(fit.rf$finalModel)

# Random Forest Prediction (Eval) (replace X)
prof_pred = predict(random_model, test)
(p = table(prof_pred, testing2$X))
CrossTable(test$X, data_pred, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('Actual X', 'Predicted X'))
(p = table(data_pred, test$X))
(Accuracy = sum(diag(p))/sum(p)*100)

## Check Error Rate (replace X)
error.rate = round(mean(data.pred != test$X,2))
error.rate

# Plot the Variable Importance ---
vi = varImp(fit.rf, scale = FALSE)
plot(vi, top = ncol(train)-1)

fancyRpartPlot(fit.rpart$finalModel)

# Confusion Matrix (replace X) ---
cm = confusionMatrix(as.factor(data.pred), reference = as.factor(test$X), mode = "prec_recall")
print(cm)
