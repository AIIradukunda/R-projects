#install.packages("csv")
#install.packages("ggplot2")
#install.packages("caTools")
library(csv)
library(ggplot2)
library(caTools)
df <- read.csv("/Users/Yash-Angelique/Desktop/2020/data/Data.csv")

# replacing missing data by  the mean 

df$Age <-ifelse(is.na(df$Age),
                ave(df$Age, FUN = function(x) mean(x,na.rm=TRUE)),
                df$Age)
df$Salary <-ifelse(is.na(df$Salary),
                ave(df$Salary, FUN = function(x) mean(x,na.rm=TRUE)),
                df$Salary)

# encode categorical data

df$Country <- factor(df$Country, 
                     levels=c('France','Spain','Germany'),
                     labels=c(1,2,3))

df$Purchased <- factor(df$Purchased, 
                     levels=c('Yes','No'),
                     labels=c(1,0))



#split dataset into training and test set
set.seed(123)

split <- sample.split(df$Purchased,SplitRatio = 0.8)
train_set <- subset(df,split==TRUE)
test_set <-subset(df, split==FALSE)


#scaling feature

train_scaling <- scale (df[,2:3])
test_scaling <- scale (df[,2:3])

# linear simple regression
library(caTools)
set.seed(123)

df_lr <- read.csv("/Users/Yash-Angelique/Desktop/2020/data/Salary_Data.csv")

split <- sample.split(df_lr$Salary,SplitRatio = 2/3)
train_set <- subset(df_lr,split==TRUE)
test_set <-subset(df_lr, split==FALSE)

#fitting simple linear regression to the training set
# ~ means that the salary is proportional to years of experience
regressor <-lm(formula=Salary ~ YearsExperience,data=train_set)

# no * means there is no statistical significance *** means there is a 
# a high statistical significance
# the lower the p-value the more significant it your independent variable 
# lower 5% means p-value  is more significant
summary(regressor)


# predict the test result

salary_pred <-predict(regressor, newdata=test_set)

# visualize the training result

ggplot()+
  geom_point(aes(x=train_set$YearsExperience, y= train_set$Salary),
             color='red')+
  geom_line((aes(x=train_set$YearsExperience, y= predict(regressor, newdata=train_set))),
                 color='blue')+
ggtitle('Salary vs YearsExperience(training model)') +
  xlab('YearsExperience')+
  ylab('Salary')


# visualize the test result 

ggplot()+
  geom_point(aes(x=test_set$YearsExperience, y= test_set$Salary),
             colour='red')+
  geom_line((aes(x=train_set$YearsExperience, y= predict(regressor, newdata=train_set))),
                 colour='blue')+
  ggtitle('Salary vs YearsExperience (testing model)') +
  xlab('YearsExperience')+
  ylab('Salary')