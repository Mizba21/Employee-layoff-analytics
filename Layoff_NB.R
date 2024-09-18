# Importing data set and viewing it
df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
df=na.omit(df)
table(head(df))
dim(df)
names(df)

# Data pre-processing
df$Age=as.factor(df$Age)
df$Gender=as.factor(df$Gender)
df$Department=as.factor(df$Department)
df$NumCompaniesWorked=as.factor(df$NumCompaniesWorked)
df$YearsAtCompany=as.factor(df$YearsAtCompany)


# Installation of required packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

library(e1071)
library(caTools)
library(caret)

# Splitting data into training and testing data
split <- sample.split(df, SplitRatio = 0.7)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Fitting the training data to a Naive Bayes Model
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Layoff ~ Age+Gender+YearsAtCompany+Department+MonthlyIncome+NumCompaniesWorked, data = train_cl)
classifier_cl


# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
y_pred

# Confusion Matrix and accuracy 
cm <- table(test_cl$Layoff, y_pred)
cm

ac_Test=sum(diag(cm)) / sum(cm)*100
print(paste('Accuracy for test is found to be', ac_Test))
