#load Packages
if(!require(rpart)){install.packages("rpart")}
library(rpart)

if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot)

if(!require(caret)){install.packages("caret")}
library(caret)

# Load Dataset
titanicDT <- read.csv("titanicDT_complete.csv", header = T)

#Survive variable into factor
titanicDT$Survived <- as.factor(titanicDT$Survived)

#setting seed
set.seed(454)

str(titanicDT)

#Splitting the data into 65% training and 35% testing
table(titanicDT$Survived)

trainRowNumbers <- createDataPartition(titanicDT$Survived, p=0.65, list=FALSE)

#Creating a training data
trainData <- titanicDT [trainRowNumbers,]

#Creating a testing data
testData <- titanicDT [-trainRowNumbers,]
mean(testData$Age)

#Creating a decision tree
trctrl <- rpart.control(maxdepth = 5 , minsplit = 100)

model <- rpart (Survived ~ Pclass +
                  Sex +
                  Age +
                  SibSp +
                  Parch +
                  Fare +
                  Embarked,
                data = trainData,
                control = trctrl,
                method = "class")

#Plotting Decision Tree
rpart.plot(model, cex = 0.8)

#Implementing model on testing set to predict
predict_test <- predict(model, newdata = testData, type = "class")

# check the confusion matrix
table(predict_test, testData$Survived)

#Alternatively checking the confusion matric
confusionMatrix(predict_test, testData$Survived)

#Load the data creditdata_logistic
creditDT <- read.csv("creditdata_logistic.csv", header = T)
set.seed(454)


#Partition the data for good loan
trainNumbers <- createDataPartition(creditDT$Good.Loan, p=0.8, list=FALSE)

#creating a train data
trainDataG <- creditDT[trainNumbers,]

#creating a test data
testDataG <- creditDT[-trainNumbers,]
mean(testDataG$Age.in.years)

#creating a logistic regression for goodloan using all other variables as predictors
trainDataG.glm <- glm(Good.Loan ~ . ,
                     data = trainDataG,
                     family = binomial)
options(scipen = 999)
summary(trainDataG.glm)

#Using the model to predict testing data set 
creditPrediction <- predict(trainDataG.glm, newdata = testDataG, type = "response")

credit.predict <- ifelse(creditPrediction > 0.5,1,0)

# create confusion matrix
table(testDataG$Good.Loan, credit.predict)
