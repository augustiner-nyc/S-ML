# Build Logistic Regression (Iris Data)

#Load the Data
library("datasets")
library("caret")
library("quantmod")
library("stepPlr")
library("caTools")
data("iris")
iris = iris
str(iris)

#Visualize Data
with(iris, qplot(iris[,1], iris[,2], colour=iris[,ncol(iris)], cex=0.2))

#Split the Data
index = createDataPartition(iris[,1], p =0.70, list = FALSE)
dim(index)

#Train the Data
training = iris[index,]
dim(training)

valid = iris[-index,]
dim(valid)

#Create Test Harness
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Build Log Regression using multinom
set.seed(7)
fit.LR <- train(Species~., data = training, method = "multinom", family=binomial(), trControl=control, metric=metric)

#Summarize results
fit.LR
summary(fit.LR$finalModel)

#Plot the model
plot(fit.LR)

#Create Predicion Using trained Regression
data.pred = predict(fit.LR, newdata = valid)
table(data.pred, valid$Species)

#Check Error
error.rate = round(mean(data.pred != valid$Species,2))
error.rate

#Confusion Matrix
cm = confusionMatrix(as.factor(data.pred), reference = as.factor(valid$Species), mode = "prec_recall")
print(cm)

#########

#Build Logistic Regression using LogitBoost
set.seed(7)
fit.LogitBoost <- train(Species~., data = training, method="LogitBoost", metric=metric, trControl=control)


#Summarize Results
fit.LogitBoost
summary(fit.LogitBoost$finalModel)

#Plot the model
plot(fit.LogitBoost)

#Create Prediction
data.pred = predict(fit.LogitBoost, newdata = valid)
table(data.pred, valid$Species)

#Cgeacj Error
error.rate = round(mean(data.pred != valid$Species,2))
error.rate

#Confusion Matrix
cm = confusionMatrix(as.factor(data.pred), reference = as.factor(valid$Species), mode = "prec_recall")
print(cm)

##########

#Build Logistic Regression using plr
set.seed(7)
fit.plr <- train(Species~., data = training, method="plr", metric=metric, trControl=control)


#Summarize Results
fit.plr
summary(fit.plr$finalModel)

#Plot the model
plot(fit.plr)

#Create Prediction
data.pred = predict(fit.plr, newdata = valid)
table(data.pred, valid$Species)

#Cgeacj Error
error.rate = round(mean(data.pred != valid$Species,2))
error.rate

#Confusion Matrix
cm = confusionMatrix(as.factor(data.pred), reference = as.factor(valid$Species), mode = "prec_recall")
print(cm)

#############

#Compare all models
results = resamples(list(LogR = fit.LR, LogitBoost=fit.LogitBoost, PLR = fit.plr))
summary(results)

#Visualize Comparison
dotplot(results)