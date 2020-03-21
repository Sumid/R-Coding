if(!require(DataExplorer)){install.packages("DataExplorer")}
library(DataExplorer)


#load the data set
my_credit <- read.csv("credit.csv", header = TRUE)

#missing values in the dataset
plot_intro(my_credit)

#plot_missing(my_credit)

#barplot of the purpose variable
barplots.my_credit <- plot_bar(my_credit$purpose)

#box plot of continuous variable cut by housing
plotbox <- plot_boxplot(my_credit, by = "housing")

#load the titianic dataset 
titanic <- read.csv("titanic_missing.csv", header = TRUE)
titanic
#Replace the missing variables in Age with median
titanic$Age <- replace(titanic$Age, is.na(titanic$Age), median(titanic$Age, na.rm = TRUE))
titanic$Age

#Replace the missing values in sib&p with 0
titanic$SibSp <- replace(titanic$SibSp, is.na(titanic$SibSp), 0)
titanic$SibSp
median(titanic$SibSp)

#fare values missing
new_titanic <- titanic[complete.cases(titanic),]
new_titanic
