#Load the package caret
if(!require(caret)){install.packages("caret")}
library(caret)

#load Dataset
winedata <- read.csv("wineKNN.csv", header = T)

#Variable into factor
winedata$Origin <- as.factor(winedata$Origin)

#Setting seed 
set.seed(234)
str(winedata$Origin)

#Splitting the data 
trainRowNumbers <- createDataPartition(winedata$Origin, p = 0.8, list = FALSE)

#creating a training data
trainData <- winedata[trainRowNumbers,]

#creating a testing data
testData <- winedata[-trainRowNumbers,]
mean(testData$Alcohol)

#Training the KNN model
knn_model <- train(Origin ~ ., data = trainData, method = "knn",
                   preProcess = c ("center","scale"))



#output of knn model
knn_model

#Implementing model on testing set to predict
knn_test <- predict(knn_model, newdata = testData)

# check the confusion matrix
confusionMatrix(knn_test, testData$Origin)

#loading more packages
if(!require(rpart)){install.packages("rpart")}
library(rpart)

if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot)

#Load dataset
titanicDT <- read.csv("titanicDT_complete.csv", header = T)

#creating a decision tree
trctrl <- rpart.control(maxdepth = 2, minsplit = 30)

model <- rpart (Survived ~ Pclass +
                  Sex +
                  Age +
                  SibSp +
                  Parch +
                  Fare +
                  Embarked,
                data = titanicDT,
                control = trctrl,
                method = "class")

#plotting decision tree
rpart.plot(model, cex = 0.8)

