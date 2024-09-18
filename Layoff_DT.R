df=read.csv("C:/Users/Dell/Downloads/finalds.csv",stringsAsFactors=T)
drop <- c("Attrition","Over18")
df = df[,!(names(df) %in% drop)]
df=na.omit(df)
head(df)
dim(df)

df$Age=as.factor(df$Age)
df$Gender=as.factor(df$Gender)
df$Department=as.factor(df$Department)
df$NumCompaniesWorked=as.factor(df$NumCompaniesWorked)
df$YearsAtCompany=as.factor(df$YearsAtCompany)


#Age, Gender, YearsAtCompany, YearsSinceLastPromotion, Department
#NumCompaniesWorkd, MonthlyIncome


install.packages("caTools")
install.packages("party")

library(caTools)
library(party)

# Splitting data in train and test data
split <- sample.split(df, SplitRatio = 0.8)
split

train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")

# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed

model<- ctree(Layoff ~ Age+Gender+Department+MonthlyIncome+NumCompaniesWorked, train)
plot(model)

# testing the people who are native speakers
# and those who are not
predict_model<-predict(model, test)
predict_model

# Changing probabilities
predict_model <- ifelse(predict_model >=0.5, 1, 0)
predict_model

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test$Layoff, predict_model)
m_at

ac_Test=sum(diag(m_at)) / sum(m_at)*100
print(paste('Accuracy for test is found to be', ac_Test))

