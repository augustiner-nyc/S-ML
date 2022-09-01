#Regression in R and Python (Google Example)

library(caret)
library(quantmod)

#Load Data
start <- as.Date(Sys.Date()-(365*5))
end <- as.Date(Sys.Date())
getSymbols("GOOG", src = "yahoo", from = start, to = end)

data = GOOG
colnames(data) = c("Open", "High", "Low", "Close", "Volume", "Adjusted")

#Visualize the Data
plot(data[, "Close"], main = "Close Price")

#Create Test Harness
control <- trainControl(method="cv", number=10)
metric <- "Rsquared"

#Split the Data
split<-createDataPartition(y = data$Close, p = 0.7, list = FALSE)
train<-data[split,]
valid<-data[-split,]

#Build Linear Regression Model(lm)
set.seed(7)
fit.LM <- train(Close~., data = train, method = "lm", trControl=control, metric=metric)

#Summarize Results
fit.LM
summary(fit.LM$finalModel)

#Create Prediction Using Regression
predictedValues<-predict(fit.LM, valid)
modelvalues<-data.frame(obs = valid$Close, pred=predictedValues)
postResample(pred = predictedValues, obs = valid$Close)
