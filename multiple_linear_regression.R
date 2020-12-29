library(csv)
library(ggplot2)
library(caTools)
set.seed(123)
df <- read.csv("/Users/Yash-Angelique/Desktop/2020/data/50_Startups.csv")

# encode categorical data
df$State <- factor(df$State, 
                     levels=c('New York','California','Florida'),
                     labels=c(1,2,3))

# split train set and test set
split <- sample.split(df$Profit,SplitRatio = 0.8)
train_set <- subset(df,split==TRUE)
test_set <-subset(df, split==FALSE)

#fitting multiple linear regression to the training set
# ~ means that the salary is proportional to years of experience
regressor <-lm(formula=Profit ~ R.D.Spend+Administration+Marketing.Spend+State,data=train_set)

# 0r
regressor <-lm(formula=Profit ~ . ,data=train_set)
summary(regressor)

# predict the test result

profit_pred <-predict(regressor, newdata=test_set)
