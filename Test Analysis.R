# A3: Binary Classification with a Bank Churn Dataset - Tom Schmidt

# Install required packages
install.packages("Amelia")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caTools")
install.packages("caret")

setwd("/Users/ts/Desktop/M. Sc. Business Analytics/02_Modules/08_Visualizing & Analyzing Data with R/02_Assignments/A3 - Binary Classification with Bank Churn Dataset")

bankchurn <- read.csv("BankChurnDataset.csv")
summary(bankchurn)
head(bankchurn, 10)



# Part 1:
# Handle missing values in the data sets

# Looking for missing values
any(is.na(bankchurn)) # has missing values
any(is.na(newcustomers)) # has no missing values

# Get all entries that contain a NA
na_rows <- bankchurn[rowSums(is.na(bankchurn)) > 0, ]
na_rows

# Replace NA's with median's of columns
bankchurn$Age[is.na(bankchurn$Age)] <- 37 # replaced with median age
bankchurn$EstimatedSalary[is.na(bankchurn$EstimatedSalary)] <- 117948 # replaced with median estimated salary

summary(bankchurn)

# Data exploration with visualizations
ggplot(bankchurn,aes(Gender)) + geom_bar(aes(fill=factor(Gender)),alpha=0.5) # Gender comparison
ggplot(bankchurn,aes(Geography)) + geom_bar(aes(fill=factor(Geography)),alpha=0.5) # Geography comparison
plot(bankchurn$Age, bankchurn$CreditScore, 
     main = "Age vs. Credit Score",
     xlab = "Age",
     ylab = "Credit Score",
     col = "blue",
     pch = 16)
# the scatterplot shows that there is no high correlation between age and credit score as many younger people also have a
# high credit score as well as older people also having lower ones



# Removing unwanted columns
library(dplyr)
df.bankchurn <- select(bankchurn,-CustomerId,-Surname)
df.bankchurn



# Part 2:
# Divide "BankChurnDataset" into training and testing datasets. Train the model and comment on accuracy, specificity and sensivity

# Training the model using logistic regression
log.model <- glm(formula=Exited ~ . , family = binomial(link='logit'),data = df.bankchurn)
summary(log.model)

# Convert columns to as.factors
df.bankchurn$Exited <- as.factor(df.bankchurn$Exited)
df.bankchurn$HasCrCard <- as.factor(df.bankchurn$HasCrCard)
df.bankchurn$IsActiveMember <- as.factor(df.bankchurn$IsActiveMember)
df.bankchurn$NumOfProducts <- as.factor(df.bankchurn$NumOfProducts) 

# Splitting df.bankchurn data set in a train and test data set
library(caTools)
set.seed(101)

split = sample.split(df.bankchurn$Exited, SplitRatio = 0.70) # use 70% as training data

df.bankchurn.train = subset(df.bankchurn, split == TRUE)
df.bankchurn.test = subset(df.bankchurn, split == FALSE)

final.log.model <- glm(formula=Exited ~ CreditScore + Geography + CreditScore + Age + Gender + Tenure + Balance + EstimatedSalary + NumOfProducts + HasCrCard + IsActiveMember , family = binomial(link='logit'),data = df.bankchurn.train) #glm = generalized linear model
summary(final.log.model)

# Check prediction accuracy
fitted.probabilities <- predict(final.log.model,newdata=df.bankchurn.test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0) 
fitted.results
fitted.results != df.bankchurn.test$Exited

# Calculate accuracy
misClasificError <- mean(fitted.results != df.bankchurn.test$Exited)
print(paste('Accuracy',1-misClasificError))
# Accuracy = 0.8562

# Creating confusion matrix
table(df.bankchurn.test$Exited, fitted.results)

# Calculate sensitivity and specificity using the confusion matrix
library(caret)
df.bankchurn.test$Exited <- factor(df.bankchurn.test$Exited)
fitted.results <- factor(fitted.results)

sensitivity(df.bankchurn.test$Exited, fitted.results)
specificity(df.bankchurn.test$Exited, fitted.results)
# Sensitivity = 0.8781
# Specificity = 0.7296

# Commenting on accuracy, sensitivity, specificity:
# An accuracy of 0.8562 means that roughly 85.62% of the predictions made by the model are correct. In this particular case
# it means that for 85% of the customers it was correctly predicted whether they would churn or not. In this domain, 
# predicting customers churn for a bank, the accuracy can be considered as quite good. However, it is impacted by the 
# imbalanced classes. If they would be less different, the accuracy would increase. 
# 
# The sensitivity, the true positive rate, indicates the proportion of actual positive instances that the created model 
# correctly identifies. 87.81% is comparably high result. In the bank churn context, a high sensitivity is helpful because
# it means that the model is effectively capturing most of the customers who are likely to churn. 
#
# The specificity, the true negative rate, indicates the proportion of actual negative instances that the created model 
# correctly identifies. In this case it means the customers who do not churn. In some cases the specificity should be 
# as high as possible when it comes to fraud detection etc. However, in our case a lower specificity of 72.96% is acceptable
# as we focus on identifying those customers who are likely to churn. 



# Part 3: 
# Use the model to predict whether a particular customer would churn or not using the "NewCustomerDataset"

newcustomers <- read.csv("NewCustomerDataset.csv")
summary(newcustomers)
head(newcustomers, 10)

# align as.factors accordingly
#newcustomers$Exited <- as.factor(newcustomers$Exited)
newcustomers$HasCrCard <- as.factor(newcustomers$HasCrCard)
newcustomers$IsActiveMember <- as.factor(newcustomers$IsActiveMember)
newcustomers$NumOfProducts <- as.factor(newcustomers$NumOfProducts)

# Apply the linear regression model to the new data
predictions <- predict(final.log.model, newdata = newcustomers)

predictions <- ifelse(predictions > 0.5,1,0)
predictions

new_customers_churn <- sum(predictions == 1)
new_customers_churn

# --> 10903 people are likely to churn












